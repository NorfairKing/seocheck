{-# LANGUAGE TypeApplications #-}

module SeoCheck.OptParseSpec (spec) where

import OptEnvConf.Test
import SeoCheck.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "seocheck"
