{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SeoCheck
  ( seoCheck,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List
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
import Rainbow
import SeoCheck.OptParse
import System.Exit
import Text.HTML.DOM as HTML
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
  bytestringMaker <- byteStringMakerFromEnvironment
  mapM_ (mapM_ SB.putStr . chunksToByteStrings bytestringMaker) $ renderSEOResult $ SEOResult {seoResultPageResults = resultsMap}
  exitWith $
    if any resultBad resultsMap
      then ExitFailure 1
      else ExitSuccess

newtype SEOResult
  = SEOResult
      { seoResultPageResults :: Map Link Result
      }
  deriving (Show, Eq)

renderSEOResult :: SEOResult -> [[Chunk Text]]
renderSEOResult SEOResult {..} = mapMaybe (uncurry renderPageResult) (M.toList seoResultPageResults)

renderPageResult :: Link -> Result -> Maybe [Chunk Text]
renderPageResult link r@Result {..} =
  if resultBad r
    then Just go
    else Nothing
  where
    go :: [Chunk Text]
    go =
      intersperse
        (chunk " ")
        $ concat
          [ [chunk $ T.pack $ show (linkUri link)],
            renderStatusResult resultStatus,
            maybe [] renderDocResult resultDocResult,
            [chunk "\n"]
          ]

renderStatusResult :: HTTP.Status -> [Chunk Text]
renderStatusResult s =
  [fore col $ chunk $ T.pack $ show sci]
  where
    sci = HTTP.statusCode s
    col = if 200 <= sci && sci < 300 then green else red

renderDocResult :: DocResult -> [Chunk Text]
renderDocResult DocResult {..} =
  [ case docResultTitle of
      NoTitleFound -> fore red $ chunk "No title"
      EmptyTitle -> fore red $ chunk "Empty title"
      NonStandardTitle e -> fore red $ chunk $ T.pack $ "Non-standard title" <> show e
      TitleFound t -> fore green $ chunk t
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

data Link = Link {linkType :: LinkType, linkUri :: URI}
  deriving (Show, Eq, Ord)

data LinkType = A | IMG | LINK
  deriving (Show, Eq, Ord)

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
           in 200 <= sci && sci < 300,
        decorate "Doc result" $ maybe valid docResultValidation resultDocResult
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
          let headers = responseHeaders resp
              contentType = lookup hContentType headers
          pure $ Just $
            Result
              { resultStatus = responseStatus resp,
                resultDocResult = case linkType link of
                  A -> do
                    ct <- contentType
                    if "text/html" `SB.isInfixOf` ct
                      then Just $ produceDocResult root $ HTML.parseLBS body
                      else Nothing
                  _ -> Nothing
              }

data DocResult
  = DocResult
      { docResultLinks :: ![Link],
        docResultTitle :: !TitleResult
      }
  deriving (Show, Eq)

docResultValidation :: DocResult -> Validation
docResultValidation DocResult {..} =
  mconcat
    [ declare "There was exactly one title" $ case docResultTitle of
        TitleFound _ -> True
        _ -> False
    ]

produceDocResult :: URI -> XML.Document -> DocResult
produceDocResult root d =
  DocResult
    { docResultLinks = documentLinks root d,
      docResultTitle = documentTitle d
    }

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

data TitleResult
  = NoTitleFound
  | EmptyTitle
  | TitleFound Text
  | NonStandardTitle Element
  deriving (Show, Eq)

documentTitle :: Document -> TitleResult
documentTitle d = case findDocumentTag (== "head") d >>= findElementTag (== "title") of
  Nothing -> NoTitleFound
  Just e@Element {..} -> case elementNodes of
    [] -> EmptyTitle
    [NodeContent t] -> TitleFound t
    _ -> NonStandardTitle e

findDocumentTag :: (Name -> Bool) -> Document -> Maybe Element
findDocumentTag p = findElementTag p . documentRoot

findElementTag :: (Name -> Bool) -> Element -> Maybe Element
findElementTag p e@Element {..} =
  go <|> msum (map goNode elementNodes)
  where
    go = do
      guard (p elementName)
      pure e
    goNode :: Node -> Maybe Element
    goNode = \case
      NodeElement e' -> findElementTag p e'
      _ -> Nothing
