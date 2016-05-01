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
import Control.Monad.Writer.Strict (tell, listen, execWriter)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Shelly (shelly, verbosely, run_, fromText)
import qualified Data.Time as TIME (ZonedTime(..), formatTime, defaultTimeLocale, getZonedTime)

generateArchiveCreateFlags :: Archive -> TIME.ZonedTime -> [T.Text]
generateArchiveCreateFlags axiv ztime = execWriter $ do
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

-- Returns an empty list to indicate no pruning.
generateArchivePruneFlags :: Archive -> [T.Text]
generateArchivePruneFlags axiv = execWriter $ do
  (_, currentFlags) <- listen $ do
    unless (axiv^.keepAllWithin == mempty) $
      tell ["--keep-within=" <> axiv^.keepAllWithin]
    unless (axiv^.nKeepAtInterval.hour == 0) $
      tell ["-H=" <> T.pack (show $ axiv^.nKeepAtInterval.hour)]
    unless (axiv^.nKeepAtInterval.day == 0) $
      tell ["-d=" <> T.pack (show $ axiv^.nKeepAtInterval.day)]
    unless (axiv^.nKeepAtInterval.week == 0) $
      tell ["-w=" <> T.pack (show $ axiv^.nKeepAtInterval.week)]
    unless (axiv^.nKeepAtInterval.month == 0) $
      tell ["-m=" <> T.pack (show $ axiv^.nKeepAtInterval.month)]
    unless (axiv^.nKeepAtInterval.year == 0) $
      tell ["-y=" <> T.pack (show $ axiv^.nKeepAtInterval.year)]
  unless (currentFlags == []) $
    tell [ "-P=" <> axiv^.archivePrefix <> "-"
         , axiv^.repositoryLoc
         ]

runManifest :: T.Text -> Archive -> IO ()
runManifest borgPath axiv = do
  ztime <- TIME.getZonedTime
  shelly . verbosely . run_ (fromText borgPath) $
    ["create", "--info", "-ns"] ++ generateArchiveCreateFlags axiv ztime
  case generateArchivePruneFlags axiv of
    [] -> pure ()
    fs -> shelly . verbosely . run_ (fromText borgPath) $
            ["prune", "--info", "-ns", "--list"] ++ fs

checkConnectionThenBackup :: Configuration -> IO ()
checkConnectionThenBackup config = do
  isUnmetered <- shelly $ isUnmeteredConn
                   (config^.activeKeywords) (config^.unmeteredConnNames)
  if isUnmetered
    then mapM_ (runManifest (config^.borgBinPath)) (config^.archiveManifest)
    else error "Not on white-listed connection. Backup terminated."
