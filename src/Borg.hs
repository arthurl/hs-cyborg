-- | Main module.

module Borg
  (
  -- * Borg backup
    checkConnectionThenBackup

  -- * Error messages
  , msgErrNoValidConnection
  , msgSuccessBackupComplete

  ) where

import Borg.Data
import Borg.Connection
import Borg.Utils

import Control.Lens ((^.))
import Control.Monad (when, unless)
import Control.Monad.Writer.Strict (tell, listen, execWriter)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Exception (SomeException, throwIO, catch)
import Shelly (Sh, shelly, verbosely, run_, fromText)
import qualified Data.Time as TIME (ZonedTime(..), formatTime, defaultTimeLocale, getZonedTime)

msgErrNoValidConnection :: T.Text
msgErrNoValidConnection =
  "ERROR: Not on white-listed connection. Backup terminated."

msgSuccessBackupComplete :: T.Text
msgSuccessBackupComplete =
  "Backup completed."

generateRemotePathFlag :: Archive -> [T.Text]
generateRemotePathFlag axiv =
  case axiv^.remoteBorgPath of
    Nothing -> mempty
    Just path -> ["--remote-path=" <> path]

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

runManifest :: Verbosity -> T.Text -> Archive -> IO ()
runManifest (Verbosity vb) borgPath axiv = do
  ztime <- TIME.getZonedTime
  if vb then
    shelly . verbosely . run_ (fromText borgPath) $
      ["create", "--info", "-s", "--list", "--filter=AME"]
      ++ generateRemotePathFlag axiv ++ generateArchiveCreateFlags axiv ztime
  else
    shelly . run_ (fromText borgPath) $
      ["create"]
      ++ generateRemotePathFlag axiv ++ generateArchiveCreateFlags axiv ztime
  case (generateArchivePruneFlags axiv, vb) of
    ([], _) -> pure ()
    (fs, True) -> shelly . verbosely . run_ (fromText borgPath) $
                    ["prune", "--info", "-s", "--list"]
                    ++ generateRemotePathFlag axiv ++ fs
    (fs, False) -> shelly . run_ (fromText borgPath) $
                     ["prune"]
                     ++ generateRemotePathFlag axiv ++ fs

runPostBackupCmdS :: Verbosity -> [(T.Text, [T.Text])] -> IO ()
runPostBackupCmdS (Verbosity vb) =
  shelly . (if vb then verbosely else id) . mapM_ go
  where
    go :: (T.Text, [T.Text]) -> Sh ()
    go (cmd', args') = run_ (fromText cmd') args'

checkConnectionThenBackup :: Configuration -> IO ()
checkConnectionThenBackup config = do
  isUnmetered <- shelly $ isUnmeteredConn
                   (config^.activeKeywords) (config^.unmeteredConnNames)
  let notifyIfSet msg =
        when (config^.osxNotifications) . shelly $ notifyOSX "hs-cyborg" msg
  if isUnmetered
    then do
      ( do
          mapM_ (runManifest (config^.setVerbosity) (config^.borgBinPath))
                (config^.archiveManifest)
          runPostBackupCmdS (config^.setVerbosity) (config^.postBackupCmdS)
        ) `catch` \e -> notifyIfSet ("ERROR: " <> T.pack (show e))
                        >> throwIO (e :: SomeException)
      notifyIfSet msgSuccessBackupComplete
    else do
      notifyIfSet msgErrNoValidConnection
      error $ T.unpack msgErrNoValidConnection
