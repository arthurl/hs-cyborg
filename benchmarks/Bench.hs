-- | Description:

module Main where

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "test_name" undefined
  ]
