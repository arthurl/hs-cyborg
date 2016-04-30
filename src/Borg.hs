-- | Main module.

module Borg
  (
  -- * Borg backup
    checkConnectionThenBackup

  ) where

import Borg.Data
import Borg.Connection

import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Monoid ((<>))
import Shelly (shelly, verbosely, run_, fromText)
import qualified Data.Time as TIME (ZonedTime(..), formatTime, defaultTimeLocale, getZonedTime)

generateArchiveFlags :: Archive -> TIME.ZonedTime -> [T.Text]
generateArchiveFlags axiv ztime =
  let compressionFlag :: T.Text
      compressionFlag = "-C=" <> axiv^.compressionMethod
      excludeFlags :: [T.Text]
      excludeFlags = map ("-e=" <>) $ axiv^.fileExcludes
      formattedTime :: String
      formattedTime = TIME.formatTime TIME.defaultTimeLocale "%Y-%m-%d-%H%Mh-%Z" ztime
      repoArchiveName :: T.Text
      repoArchiveName = axiv^.repositoryLoc <> "::"
                        <> axiv^.archivePrefix <> "-"
                        <> T.pack formattedTime
      filePathsArg :: [T.Text]
      filePathsArg = map T.pack $ axiv^.filePaths
  in compressionFlag : excludeFlags ++ repoArchiveName : filePathsArg

backupArchive :: T.Text -> Archive -> IO ()
backupArchive borgPath axiv = do
  ztime <- TIME.getZonedTime
  shelly . verbosely . run_ (fromText borgPath) $
    ["create", "-nsp"] ++ generateArchiveFlags axiv ztime

runBackup :: Configuration -> IO ()
runBackup config =
  mapM_ (backupArchive (config^.borgBinPath)) (config^.archiveManifest)

checkConnectionThenBackup :: Configuration -> IO ()
checkConnectionThenBackup config = do
  isUnmetered <- shelly $ isUnmeteredConn
                   (config^.activeKeywords) (config^.unmeteredConnNames)
  if isUnmetered
    then runBackup config
    else error "Not on white-listed connection. Backup terminated."
