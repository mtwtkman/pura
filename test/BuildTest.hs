module BuildTest (props) where

import Data.List
import qualified Pura.Build as PB
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary as Q
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

props :: TestTree
props = testGroup "Pura.Build" [indentProps]

indentProps :: TestTree
indentProps =
  testGroup
    "indent"
    [ QC.testProperty "adds spaces to passed string head" $
        \i s -> splitAt i (PB.indent i (s :: String)) == (intercalate "" $ replicate i " ", s)
    ]
