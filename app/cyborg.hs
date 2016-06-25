-- |

module Main where

import Borg

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

main :: IO ()
main = do
  filePaths <- getArgs
  let  go :: FilePath -> IO ()
       go jsonFilePath = do
         eJSON <- J.eitherDecode' <$> B.readFile jsonFilePath
         case eJSON of
           Left err -> fail err
           Right rJSON -> checkConnectionThenBackup rJSON
  mapM_ go filePaths
