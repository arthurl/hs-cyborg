-- |

module Main where

import Borg

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  eJSON <- J.eitherDecode' <$> B.readFile "config.json"
  case eJSON of
    Left err -> fail err
    Right rJSON -> checkConnectionThenBackup rJSON
