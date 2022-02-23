module Main where

import Data.Coerce (coerce)
import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, checkCoverage, cover)
