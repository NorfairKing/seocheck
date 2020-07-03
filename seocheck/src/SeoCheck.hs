{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SeoCheck
  ( seoCheck,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import SeoCheck.OptParse
import System.Exit
import Text.HTML.DOM as HTML
import Text.HTML.TagSoup
import Text.XML (Document (..))
import Text.XML as XML
import UnliftIO

seoCheck :: IO ()
seoCheck = do
  Settings {..} <- getSettings
  man <- HTTP.newTlsManager
  queue <- newTQueueIO
  seen <- newTVarIO S.empty
  results <- newTVarIO M.empty
  let fetchers = fromMaybe 1 setFetchers
      indexes = [0 .. fetchers - 1]
  fetcherStati <- newTVarIO $ IM.fromList $ zip indexes (repeat True)
  atomically $ writeTQueue queue (Link A setUri)
  runStderrLoggingT $ filterLogger (\_ ll -> ll >= setLogLevel) $ do
    logInfoN $ "Running with " <> T.pack (show fetchers) <> " fetchers"
    forConcurrently_ indexes $ \ix ->
      worker setUri man queue seen results fetcherStati ix
  resultsMap <- readTVarIO results
  ( if any resultBad resultsMap
      then die
      else putStrLn
    )
    $ showResults resultsMap

showResults :: Map Link Result -> String
showResults m =
  unlines
    $ map (uncurry showResult)
    $ M.toList m

showResult :: Link -> Result -> String
showResult uri res =
  unwords
    [ show (linkUri uri) <> ":",
      show (HTTP.statusCode $ resultStatus res)
    ]

worker :: URI -> HTTP.Manager -> TQueue Link -> TVar (Set Link) -> TVar (Map Link Result) -> TVar (IntMap Bool) -> Int -> LoggingT IO ()
worker root man queue seen results stati index = go True
  where
    setStatus b = atomically $ modifyTVar' stati $ IM.insert index b
    setBusy = setStatus True
    setIdle = setStatus False
    allDone :: MonadIO m => m Bool
    allDone = all not <$> readTVarIO stati
    go busy = do
      mv <- atomically $ tryReadTQueue queue
      -- Get an item off the queue
      case mv of
        -- No items on the queue
        Nothing -> do
          -- Set this worker as idle
          logDebugN $ "Worker is idle: " <> T.pack (show index)
          when busy setIdle
          -- If all workers are idle, we are done.
          ad <- allDone
          unless ad $ do
            liftIO $ threadDelay 10000 -- 10 ms
            go False
        -- An item on the queue
        Just link -> do
          -- Set this worker as busy
          logDebugN $ "Worker is busy: " <> T.pack (show index)
          unless busy setBusy
          -- Check if the link has been seen already
          alreadySeen <- S.member link <$> readTVarIO seen
          if alreadySeen
            then do
              -- We've already seen it, don't do anything.
              logDebugN $ "Not fetching again: " <> T.pack (show link)
              pure ()
            else do
              -- We haven't seen it yet. Mark it as seen.
              atomically $ modifyTVar' seen $ S.insert link
              mres <- produceResult man root link
              forM_ mres $ \res -> do
                atomically $ modifyTVar' results $ M.insert link res
                forM_ (docResultLinks <$> resultDocResult res) $ \uris ->
                  atomically $ mapM_ (writeTQueue queue) uris
          -- Filter out the ones that are not on the same host.
          go True

data Result
  = Result
      { resultStatus :: HTTP.Status,
        resultDocResult :: Maybe DocResult
      }
  deriving (Show, Eq)

resultBad :: Result -> Bool
resultBad Result {..} =
  not $ validationIsValid $
    mconcat
      [ declare "The status code is in the 200 range" $
          let sci = HTTP.statusCode resultStatus
           in 200 <= sci && sci < 300
      ]

produceResult :: HTTP.Manager -> URI -> Link -> LoggingT IO (Maybe Result)
produceResult man root link =
  let uri = linkUri link
   in -- Create a request
      case requestFromURI uri of
        Nothing -> do
          logErrorN $ "Unable to construct a request from this uri: " <> T.pack (show uri)
          pure Nothing
        Just req -> do
          logInfoN $ "Fetching: " <> T.pack (show uri)
          -- Do the actual fetch
          resp <- liftIO $ httpLbs req man
          let status = responseStatus resp
          let sci = HTTP.statusCode status
          logDebugN $ "Got response for " <> T.pack (show uri) <> ": " <> T.pack (show sci)
          -- If the status code is not in the 2XX range, add it to the results
          let body = responseBody resp
          pure $ Just $
            Result
              { resultStatus = responseStatus resp,
                resultDocResult = case linkType link of
                  A -> Just $ produceDocResult root $ HTML.parseLBS body
                  _ -> Nothing
              }

newtype DocResult
  = DocResult
      { docResultLinks :: [Link]
      }
  deriving (Show, Eq)

produceDocResult :: URI -> XML.Document -> DocResult
produceDocResult root d =
  DocResult
    { docResultLinks = documentLinks root d
    }

data Link = Link {linkType :: LinkType, linkUri :: URI}
  deriving (Show, Eq, Ord)

data LinkType = A | IMG | LINK
  deriving (Show, Eq, Ord)

documentLinks :: URI -> Document -> [Link]
documentLinks root = elementLinks root . documentRoot

elementLinks :: URI -> Element -> [Link]
elementLinks root Element {..} =
  ( case singleElementLink root elementName elementAttributes of
      Nothing -> id
      Just l -> (l :)
  )
    $ concatMap (nodeLinks root) elementNodes

singleElementLink :: URI -> Name -> Map Name Text -> Maybe Link
singleElementLink root name attrs = do
  (typ, t) <- case name of
    "a" -> (,) A <$> M.lookup "href" attrs
    "link" -> (,) LINK <$> M.lookup "href" attrs
    "img" -> (,) IMG <$> M.lookup "src" attrs
    _ -> Nothing
  uri <- parseURIRelativeTo root $ T.unpack t
  guard $ uriAuthority uri == uriAuthority root
  pure $ Link {linkType = typ, linkUri = uri}

nodeLinks :: URI -> Node -> [Link]
nodeLinks root = \case
  NodeElement e -> elementLinks root e
  NodeContent _ -> []
  NodeComment _ -> []
  NodeInstruction _ -> []

parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo root s =
  msum
    [ (`relativeTo` root) <$> parseRelativeReference s,
      parseAbsoluteURI s
    ]

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = \case
  Left _ -> Nothing
  Right a -> Just a

aTagHref :: (Eq str, IsString str) => Tag str -> Maybe str
aTagHref = \case
  TagOpen "a" as -> lookup "href" as
  TagOpen "link" as -> lookup "href" as
  TagOpen "img" as -> lookup "src" as
  _ -> Nothing
