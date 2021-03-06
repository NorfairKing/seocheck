module SeoCheck.OptParse.Types where

import Control.Monad.Logger
import Data.Aeson hiding ((<?>))
import Network.URI
import YamlParse.Applicative

data Flags
  = Flags
      { flagUri :: !URI,
        flagLogLevel :: !(Maybe LogLevel),
        flagFetchers :: !(Maybe Int)
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
  deriving (Show, Eq)

data Environment
  = Environment
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = pure Configuration

data Settings
  = Settings
      { setUri :: !URI,
        setLogLevel :: !LogLevel,
        setFetchers :: !(Maybe Int)
      }
  deriving (Show, Eq)
