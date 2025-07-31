{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SeoCheck
  ( seoCheck,
    runSeoCheck,
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (getRedirectedRequest, httpRaw, httpRedirect)
import Network.HTTP.Client.Internal as HTTP (toHttpException)
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import SeoCheck.OptParse
import System.Exit
import Text.Colour
import Text.Colour.Term
import Text.HTML.DOM as HTML
import Text.Show.Pretty (ppShow)
import Text.XML as XML
import UnliftIO hiding (link)

seoCheck :: IO ()
seoCheck = getSettings >>= runSeoCheck

runSeoCheck :: Settings -> IO ()
runSeoCheck settings@Settings {..} = do
  man <- HTTP.newTlsManager
  queue <- newTQueueIO
  seen <- newTVarIO S.empty
  results <- newTVarIO M.empty
  let fetchers = fromMaybe 1 setFetchers
      indexes = [0 .. fetchers - 1]
  fetcherStati <- newTVarIO $ IM.fromList $ zip indexes (repeat True)
  atomically $ writeTQueue queue (Link A setUri 0)
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      logDebugN $ T.pack $ ppShow settings
      logInfoN $ "Running with " <> T.pack (show fetchers) <> " fetchers"
      forConcurrently_ indexes $ \ix ->
        worker setUri setMaxDepth man queue seen results fetcherStati ix
  resultsMap <- readTVarIO results
  putChunksLocale $ concat $ renderSEOResult $ SEOResult {seoResultPageResults = resultsMap}
  when (any resultBad resultsMap) $ exitWith $ ExitFailure 1

newtype SEOResult = SEOResult
  { seoResultPageResults :: Map Link Result
  }
  deriving (Show, Eq)

renderSEOResult :: SEOResult -> [[Chunk]]
renderSEOResult SEOResult {..} = concat $ mapMaybe (uncurry renderPageResult) (M.toList seoResultPageResults)

renderPageResult :: Link -> Result -> Maybe [[Chunk]]
renderPageResult link r@Result {..} =
  if resultBad r
    then Just go
    else Nothing
  where
    go :: [[Chunk]]
    go =
      intersperse [chunk "\n"] $
        [ [fore blue $ chunk $ T.pack $ show (linkUri link)],
          renderStatusResult resultStatus
        ]
          ++ maybe [] renderDocResult resultDocResult

renderStatusResult :: HTTP.Status -> [Chunk]
renderStatusResult s =
  [ chunk "Status: ",
    fore col $ chunk $ T.pack $ show sci
  ]
  where
    sci = HTTP.statusCode s
    col = if 200 <= sci && sci < 400 then green else red

renderDocResult :: DocResult -> [[Chunk]]
renderDocResult DocResult {..} =
  [ [ chunk "Doctype: ",
      case docResultDocType of
        HtmlDocType -> fore green $ chunk "html"
        UnknownDocType -> fore red $ chunk "Unknown doctype"
        NoDocType -> fore red $ chunk "No doctype"
    ],
    [ chunk "Title: ",
      case docResultTitle of
        NoTitleFound -> fore red $ chunk "No title"
        EmptyTitle -> fore red $ chunk "Empty title"
        NonStandardTitle e -> fore red $ chunk $ T.pack $ "Non-standard title: " <> show e
        TitleFound t -> fore green $ chunk t
    ],
    [ chunk "Description: ",
      case docResultDescription of
        Description d -> fore green $ chunk d
        EmptyDescription -> fore red $ chunk "Empty description"
        NoDescription -> fore red $ chunk "No description"
        MultipleDescriptions -> fore red $ chunk "Multiple descriptions"
        NonStandardDescription e -> fore red $ chunk $ T.pack $ "Non-standard description: " <> show e
    ],
    [ chunk "Images without Alt: ",
      case S.toList docResultImagesWithoutAlt of
        [] -> fore green $ chunk "None"
        is -> fore red $ chunk $ T.pack $ show is
    ],
    [chunk "\n"] -- Empty line
  ]

worker ::
  URI ->
  Maybe Word ->
  HTTP.Manager ->
  TQueue Link ->
  TVar (Set URI) ->
  TVar (Map Link Result) ->
  TVar (IntMap Bool) ->
  Int ->
  LoggingT IO ()
worker root maxDepth man queue seen results stati index = go True
  where
    setStatus b = atomically $ modifyTVar' stati $ IM.insert index b
    setBusy = setStatus True
    setIdle = setStatus False
    allDone :: (MonadIO m) => m Bool
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
            liftIO $ threadDelay 10_000 -- 10 ms
            go False
        -- An item on the queue
        Just link -> do
          -- Set this worker as busy
          logDebugN $ "Worker is busy: " <> T.pack (show index)
          unless busy setBusy
          -- Check if the link has been seen already
          alreadySeen <- S.member (linkUri link) <$> readTVarIO seen
          if alreadySeen
            then do
              -- We've already seen it, don't do anything.
              logDebugN $ "Not fetching again: " <> T.pack (show (linkUri link))
              pure ()
            else do
              -- We haven't seen it yet. Mark it as seen.
              atomically $ modifyTVar' seen $ S.insert (linkUri link)
              errOrRes <- produceResult root maxDepth man link
              case errOrRes of
                Left err -> do
                  logErrorN $ T.pack $ "Error fetching: " <> show (linkUri link) <> ":\n" <> err
                  liftIO exitFailure
                Right res -> do
                  atomically $ modifyTVar' results $ M.insert link res
                  let recurse = case maxDepth of
                        Nothing -> True
                        Just md -> linkDepth link < md
                  when recurse $
                    forM_ (docResultLinks <$> resultDocResult res) $ \uris ->
                      atomically $ mapM_ (writeTQueue queue) uris
          -- Filter out the ones that are not on the same host.
          go True

data Link = Link
  { linkType :: !LinkType,
    linkUri :: !URI,
    linkDepth :: !Word
  }
  deriving (Show, Eq, Ord)

data LinkType
  = A
  | IMG
  | LINK
  deriving (Show, Eq, Ord)

data Result = Result
  { resultStatus :: !HTTP.Status,
    resultDocResult :: !(Maybe DocResult)
  }
  deriving (Show, Eq)

resultBad :: Result -> Bool
resultBad Result {..} =
  not $
    validationIsValid $
      mconcat
        [ declare "The status code is in the 200 or 300 ranges" $
            let sci = HTTP.statusCode resultStatus
             in 200 <= sci && sci < 400,
          decorate "Doc result" $ maybe valid docResultValidation resultDocResult
        ]

produceResult :: URI -> Maybe Word -> HTTP.Manager -> Link -> LoggingT IO (Either String Result)
produceResult root maxDepth man link@Link {..} =
  -- Create a request
  case requestFromURI linkUri of
    Nothing -> pure $ Left $ "Unable to construct a request from this uri: " <> show linkUri
    Just req -> do
      let fetchingLog = case maxDepth of
            Nothing -> ["Fetching: ", show linkUri]
            Just md -> ["Depth ", show linkDepth, "/", show md, "; Fetching: ", show linkUri]
      logInfoN $ T.pack $ concat fetchingLog
      -- Do the actual fetch
      errOrResp <- liftIO $ retryHTTP req $ httpWithRedirects root req man
      case errOrResp of
        Left e -> pure $ Left $ "Error fetching " <> show linkUri <> ": " <> show e
        Right resp -> do
          let status = responseStatus resp
          let sci = HTTP.statusCode status
          logDebugN $ "Got response for " <> T.pack (show linkUri) <> ": " <> T.pack (show sci)
          -- If the status code is not in the 2XX range, add it to the results
          let body = responseBody resp
          let headers = responseHeaders resp
              contentType = lookup hContentType headers
          pure $
            Right $
              Result
                { resultStatus = responseStatus resp,
                  resultDocResult = case linkType of
                    A -> do
                      ct <- contentType
                      if "text/html" `SB.isInfixOf` ct
                        then Just $ produceDocResult link resp $ HTML.parseLBS body
                        else Nothing
                    _ -> Nothing
                }

data DocResult = DocResult
  { docResultLinks :: ![Link],
    docResultDocType :: !DocTypeResult,
    docResultTitle :: !TitleResult,
    docResultDescription :: !DescriptionResult,
    docResultImagesWithoutAlt :: !(Set Text) -- The 'src' tags of those images
  }
  deriving (Show, Eq)

docResultValidation :: DocResult -> Validation
docResultValidation DocResult {..} =
  mconcat
    [ declare "There was a doctype" $ case docResultDocType of
        HtmlDocType -> True
        NoDocType -> False
        UnknownDocType -> False,
      declare "There was exactly one title" $ case docResultTitle of
        TitleFound _ -> True
        _ -> False,
      declare "There was exactly one description" $
        case docResultDescription of
          Description _ -> True
          _ -> False,
      declare "There are no pages without alt tags" $ S.null docResultImagesWithoutAlt
    ]

produceDocResult :: Link -> Response LB.ByteString -> XML.Document -> DocResult
produceDocResult link resp d =
  DocResult
    { docResultLinks = documentLinks link d,
      docResultDocType = documentDocType resp,
      docResultTitle = documentTitle d,
      docResultDescription = documentDescription d,
      docResultImagesWithoutAlt = documentImagesWithoutAlt d
    }

documentLinks :: Link -> Document -> [Link]
documentLinks link = elementLinks link . documentRoot

elementLinks :: Link -> Element -> [Link]
elementLinks link Element {..} =
  ( case singleElementLink link elementName elementAttributes of
      Nothing -> id
      Just l -> (l :)
  )
    $ concatMap (nodeLinks link) elementNodes

singleElementLink :: Link -> Name -> Map Name Text -> Maybe Link
singleElementLink link name attrs = do
  (typ, t) <- case name of
    "a" -> (,) A <$> M.lookup "href" attrs
    "link" -> (,) LINK <$> M.lookup "href" attrs
    "img" -> (,) IMG <$> M.lookup "src" attrs
    _ -> Nothing
  let root = linkUri link
  uri <- parseURIRelativeTo root $ T.unpack t
  -- We remove the fragment so that the same uri (with different fragment) is not fetched twice.
  guard $ sameDomainPredicate root uri
  let uri' = uri {uriFragment = ""}
  pure $
    Link
      { linkType = typ,
        linkUri = uri',
        linkDepth = succ (linkDepth link)
      }

sameDomainPredicate :: URI -> URI -> Bool
sameDomainPredicate root uri =
  uriAuthority uri == uriAuthority root

nodeLinks :: Link -> Node -> [Link]
nodeLinks link = \case
  NodeElement e -> elementLinks link e
  NodeContent _ -> []
  NodeComment _ -> []
  NodeInstruction _ -> []

parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo root s =
  msum
    [ (`relativeTo` root) <$> parseRelativeReference s,
      parseAbsoluteURI s
    ]

data DocTypeResult = HtmlDocType | NoDocType | UnknownDocType
  deriving (Show, Eq)

documentDocType :: Response LB.ByteString -> DocTypeResult
documentDocType resp =
  if CI.mk (LB.take (fromIntegral $ SB.length "<!DOCTYPE ") (responseBody resp)) == CI.mk "<!DOCTYPE "
    then
      if CI.mk (LB.take (fromIntegral $ SB.length "<!DOCTYPE html>") (responseBody resp)) == CI.mk "<!DOCTYPE html>"
        then HtmlDocType
        else UnknownDocType
    else NoDocType

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

data DescriptionResult
  = NoDescription
  | EmptyDescription
  | MultipleDescriptions
  | Description Text
  | NonStandardDescription Element
  deriving (Show, Eq)

documentDescription :: Document -> DescriptionResult
documentDescription d =
  case findDocumentTag (== "head") d of
    Nothing -> NoDescription
    Just headTag ->
      let metaTags = findElementTags (== "meta") headTag
          isMetaDescription e = M.lookup "name" (elementAttributes e) == Just "description"
       in case filter isMetaDescription metaTags of
            [] -> NoDescription
            [e] -> case elementNodes e of
              [] -> maybe EmptyDescription Description $ M.lookup "content" (elementAttributes e)
              _ -> NonStandardDescription e
            _ -> MultipleDescriptions

findDocumentTag :: (Name -> Bool) -> Document -> Maybe Element
findDocumentTag p = findElementTag p . documentRoot

documentImagesWithoutAlt :: Document -> Set Text
documentImagesWithoutAlt d = S.fromList $
  flip mapMaybe (findDocumentTags (== "img") d) $
    \e -> do
      src <- M.lookup "src" (elementAttributes e) -- We skip the ones without a 'src' attribute because we cannot identify them.
      case M.lookup "alt" (elementAttributes e) of
        Nothing -> Just src
        Just "" -> Just src
        Just a -> if T.null (T.strip a) then Just src else Nothing

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

findDocumentTags :: (Name -> Bool) -> Document -> [Element]
findDocumentTags p = findElementTags p . documentRoot

findElementTags :: (Name -> Bool) -> Element -> [Element]
findElementTags p e@Element {..} =
  go (concatMap goNode elementNodes)
  where
    go = if p elementName then (e :) else id
    goNode :: Node -> [Element]
    goNode = \case
      NodeElement e' -> findElementTags p e'
      _ -> []

retryHTTP ::
  -- | Just  for the error message
  Request ->
  IO (Response a) ->
  IO (Either HttpException (Response a))
retryHTTP req action =
  let policy =
        mconcat
          [ exponentialBackoff 100_000,
            limitRetries 3
          ]
   in retrying
        policy
        (\_ e -> pure (couldBeFlaky e))
        ( \_ ->
            (Right <$> action)
              `catches` [ Handler $ pure . Left,
                          Handler $ pure . Left . toHttpException req
                        ]
        )
  where
    couldBeFlaky (Left e) = case e of
      HttpExceptionRequest _ hec -> case hec of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        ConnectionFailure _ -> True
        NoResponseDataReceived -> True
        _ -> False
      InvalidUrlException _ _ -> False
    couldBeFlaky _ = False

httpWithRedirects :: URI -> Request -> HTTP.Manager -> IO (Response LB.ByteString)
httpWithRedirects root request man = httpRedirect 10 go request >>= consumeBody
  where
    go :: HTTP.Request -> IO (Response HTTP.BodyReader, Maybe HTTP.Request)
    go r = do
      response <- httpRaw r man
      pure
        ( response,
          do
            newReq <-
              getRedirectedRequest
                request
                r
                (responseHeaders response)
                (responseCookieJar response)
                (statusCode (responseStatus response))
            guard $ sameDomainPredicate root (getUri newReq)
            pure newReq
        )

    consumeBody res = do
      bss <- brConsume $ responseBody res
      return res {responseBody = LB.fromChunks bss}
