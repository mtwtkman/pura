module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified BuildTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [BuildTest.props]
