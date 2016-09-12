module Main (main) where

import Data.List                            (intercalate)
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (Assertion,(@?=))
import Data.Text.Number                     (integralToText)
import Data.Word

import qualified Data.Text as Text

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Correctness"
    [ testProperty "Isomorphism" integralToTextMatchShow
    ]
  ]

integralToTextMatchShow :: Word8 -> Bool
integralToTextMatchShow a = show a == Text.unpack (integralToText a)

