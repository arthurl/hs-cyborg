{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (main) where

import Test.Tasty
import qualified Test.Tasty.HUnit as U
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.QuickCheck.Function as Q


----- module1 tests ------------------------------

tests_module1 :: TestTree
tests_module1 = testGroup "module1"
  [
  ]


------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "module_name library tests"
  [ tests_module1
  ]
