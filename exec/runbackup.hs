-- |

module Main where

import Borg.Data
import Borg

configRemote :: Configuration
configRemote = undefined

main :: IO ()
main = checkConnectionThenBackup configRemote
