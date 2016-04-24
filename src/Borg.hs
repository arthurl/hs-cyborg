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
import Control.Monad (when)
import Data.Monoid ((<>))
import Shelly (shelly, verbosely, run_, escaping)
import qualified Shelly as S (FilePath)
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

-- Note: escaping needs to be False, otherwise file exclusion globs will be escaped.
backupArchive :: S.FilePath -> Archive -> IO ()
backupArchive borgPath axiv = do
  ztime <- TIME.getZonedTime
  shelly . verbosely . escaping False . run_ borgPath $
    ["create", "-nsp"] ++ generateArchiveFlags axiv ztime

runBackup :: Configuration -> IO ()
runBackup config =
  mapM_ (backupArchive (config^.borgBinPath)) (config^.archiveManifest)

checkConnectionThenBackup :: Configuration -> IO ()
checkConnectionThenBackup config = do
  isUnmetered <- shelly $ isUnmeteredConn
                   (config^.activeKeywords) (config^.unmeteredConnNames)
  when isUnmetered $ runBackup config
