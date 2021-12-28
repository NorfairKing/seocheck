module SeoCheck.OptParse.Types where

import Control.Monad.Logger
import Network.URI

data Flags = Flags
  { flagUri :: !URI,
    flagLogLevel :: !(Maybe LogLevel),
    flagFetchers :: !(Maybe Int),
    flagMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setUri :: !URI,
    setLogLevel :: !LogLevel,
    setFetchers :: !(Maybe Int),
    setMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)
