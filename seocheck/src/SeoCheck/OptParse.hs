{-# LANGUAGE RecordWildCards #-}

module SeoCheck.OptParse
  ( module SeoCheck.OptParse,
    module SeoCheck.OptParse.Types,
  )
where

import Control.Monad.Logger
import Data.Maybe
import Network.URI
import Options.Applicative
import SeoCheck.OptParse.Types
import qualified System.Environment as System
import Text.Read

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  deriveSettings flags

deriveSettings :: Flags -> IO Settings
deriveSettings Flags {..} = do
  let setUri = flagUri
      setLogLevel = fromMaybe LevelWarn flagLogLevel
      setFetchers = flagFetchers
      setMaxDepth = flagMaxDepth
  pure Settings {..}

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Flags
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) fullDesc

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> argument
      (maybeReader parseAbsoluteURI)
      ( mconcat
          [ help "The root uri. This must be an absolute URI. For example: https://example.com or http://localhost:8000",
            metavar "URI"
          ]
      )
    <*> option
      (Just <$> maybeReader parseLogLevel)
      ( mconcat
          [ long "log-level",
            help $ "The log level, example values: " <> show (map (drop 5 . show) [LevelDebug, LevelInfo, LevelWarn, LevelError]),
            metavar "LOG_LEVEL",
            value Nothing
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "fetchers",
            help "The number of threads to fetch from. This application is usually not CPU bound so you can comfortably set this higher than the number of cores you have",
            metavar "INT",
            value Nothing
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "max-depth",
            help "The maximum length of the path from the root to a given URI",
            metavar "INT",
            value Nothing
          ]
      )

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s
