-- |

module Main where

import Borg

import Data.Maybe (fromJust)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = fromJust . J.decode <$> B.readFile "config.json" >>= checkConnectionThenBackup
