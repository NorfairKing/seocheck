{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module SeoCheck.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Control.Monad.Logger
import Network.URI
import OptEnvConf
import Paths_seocheck (version)

getSettings :: IO Settings
getSettings = runSettingsParser version

data Settings = Settings
  { setUri :: !URI,
    setLogLevel :: !LogLevel,
    setFetchers :: !(Maybe Int),
    setMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)

instance HasParser Settings where
  settingsParser = do
    setUri <-
      setting
        [ help "The root uri. This must be an absolute URI. example: http://localhost:8000",
          reader $ maybeReader parseAbsoluteURI,
          argument,
          env "ROOT",
          metavar "URI"
        ]
    setLogLevel <-
      setting
        [ help "Minimal severity of log messages",
          value LevelWarn,
          reader auto,
          option,
          long "log-level",
          metavar "LOG_LEVEL"
        ]
    setFetchers <-
      optional $
        setting
          [ help "The number of threads to fetch from. This application is usually not CPU bound so you can comfortably set this higher than the number of cores you have",
            reader auto,
            name "fetchers",
            metavar "NUM"
          ]
    setMaxDepth <-
      optional $
        setting
          [ help "The maximum length of the path from the root to a given URI",
            reader auto,
            name "max-depth",
            metavar "NUM"
          ]
    pure Settings {..}
