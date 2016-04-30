-- | Main module.

module Borg
  (
  -- * Borg backup
    checkConnectionThenBackup

  ) where

import Borg.Data
import Borg.Connection

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Writer.Strict (tell, execWriter)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Shelly (shelly, verbosely, run_, fromText)
import qualified Data.Time as TIME (ZonedTime(..), formatTime, defaultTimeLocale, getZonedTime)

generateArchiveFlags :: Archive -> TIME.ZonedTime -> [T.Text]
generateArchiveFlags axiv ztime = execWriter $ do
  unless (axiv^.compressionMethod == mempty) $
    tell ["-C=" <> axiv^.compressionMethod]
  unless (axiv^.fileExcludes == mempty) $
    tell $ map ("-e=" <>) $ axiv^.fileExcludes
  case axiv^.chunkerParams of
    Nothing -> pure ()
    Just (c1,c2,c3,c4) -> tell [ "--chunker-params="
                               <> T.pack (show c1) <> T.pack (',' : show c2)
                               <> T.pack (',' : show c3) <> T.pack (',' : show c4)
                               ]
  let formattedTime :: String
      formattedTime = TIME.formatTime TIME.defaultTimeLocale "%Y-%m-%d-%H%Mh-%Z" ztime
  tell [ axiv^.repositoryLoc <> "::"
       <> axiv^.archivePrefix <> "-" <> T.pack formattedTime
       ]
  tell . map T.pack $ axiv^.filePaths

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
