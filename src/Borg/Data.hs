-- | A module exporting data structures.

module Borg.Data
  (
  -- * Data structures
    Interval(..)
  , Archive(..)
  , Verbosity(..)
  , Configuration(..)

  -- * Lenses and Prisms
  -- ** Interval
  , hour
  , day
  , week
  , month
  , year

  -- ** Archive
  , repositoryLoc
  , chunkerParams
  , compressionMethod
  , archivePrefix
  , filePaths
  , fileExcludes
  , keepAllWithin
  , nKeepAtInterval

  -- ** Verbosity
  , iVbBool

  -- ** Configuration
  , activeKeywords
  , unmeteredConnNames
  , osxNotifications
  , setVerbosity
  , borgBinPath
  , archiveManifest
  , postBackupCmdS
  ) where

import Control.Lens (Lens', Iso', iso)
import qualified Data.Text as T
import qualified Data.Aeson as J (FromJSON, parseJSON, withObject, withBool)
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Vector as V (toList)

data Interval = Interval
  { _hour  :: Int
  , _day   :: Int
  , _week  :: Int
  , _month :: Int
  , _year  :: Int
  } deriving (Show)

hour :: Lens' Interval Int
hour f t =
  (\p' -> t {_hour = p'}) <$> f (_hour t)

day :: Lens' Interval Int
day f t =
  (\p' -> t {_day = p'}) <$> f (_day t)

week :: Lens' Interval Int
week f t =
  (\p' -> t {_week = p'}) <$> f (_week t)

month :: Lens' Interval Int
month f t =
  (\p' -> t {_month = p'}) <$> f (_month t)

year :: Lens' Interval Int
year f t =
  (\p' -> t {_year = p'}) <$> f (_year t)

instance J.FromJSON Interval where
  parseJSON = J.withObject "Interval" $ \o ->
    Interval <$> o .:? "hourly" .!= 0
             <*> o .:? "daily" .!= 0
             <*> o .:? "weekly" .!= 0
             <*> o .:? "monthly" .!= 0
             <*> o .:? "yearly" .!= 0

data Archive = Archive
  { _repositoryLoc           :: T.Text
      -- ^ Loc
  , _chunkerParams           :: Maybe (Int, Int, Int, Int)
      -- ^ Chunking parameters. See borg documentation for usage.
  , _compressionMethod       :: T.Text
      -- ^ Select compression algorithm and level. If using borg, from borg
      -- docs:
      --
      -- @
      -- none == no compression (default), lz4 == lz4, zlib == zlib (default
      -- level 6), zlib,0 .. zlib,9 == zlib (with level 0..9), lzma == lzma
      -- (default level 6), lzma,0 .. lzma,9 == lzma (with level 0..9).
      -- @
  , _archivePrefix           :: T.Text
      -- ^ Prefix to attach to archive name.
  , _filePaths               :: [FilePath]
      -- ^ List of files to archive.
  , _fileExcludes            :: [T.Text]
      -- ^ List of file globs to exclude.
  , _keepAllWithin           :: T.Text
      -- ^ Archives will never be pruned before this time.
  , _nKeepAtInterval         :: Interval
      -- ^ Number of archives to keep for the given time intervals.
  } deriving (Show)

repositoryLoc :: Lens' Archive T.Text
repositoryLoc f t =
  (\p' -> t {_repositoryLoc = p'}) <$> f (_repositoryLoc t)

compressionMethod :: Lens' Archive T.Text
compressionMethod f t =
  (\p' -> t {_compressionMethod = p'}) <$> f (_compressionMethod t)

chunkerParams :: Lens' Archive (Maybe (Int, Int, Int, Int))
chunkerParams f t =
  (\p' -> t {_chunkerParams = p'}) <$> f (_chunkerParams t)

archivePrefix :: Lens' Archive T.Text
archivePrefix f t =
  (\p' -> t {_archivePrefix = p'}) <$> f (_archivePrefix t)

filePaths :: Lens' Archive [FilePath]
filePaths f t =
  (\p' -> t {_filePaths = p'}) <$> f (_filePaths t)

fileExcludes :: Lens' Archive [T.Text]
fileExcludes f t =
  (\p' -> t {_fileExcludes = p'}) <$> f (_fileExcludes t)

keepAllWithin :: Lens' Archive T.Text
keepAllWithin f t =
  (\p' -> t {_keepAllWithin = p'}) <$> f (_keepAllWithin t)

nKeepAtInterval :: Lens' Archive Interval
nKeepAtInterval f t =
  (\p' -> t {_nKeepAtInterval = p'}) <$> f (_nKeepAtInterval t)

instance J.FromJSON Archive where
  parseJSON = J.withObject "Archive" $ \o -> do
    pruneObj <- o .:? "pruningConfig" .!= mempty
    Archive <$> o .: "repositoryLoc"
            <*> o .:? "chunkerParams"
            <*> o .:? "compressionMethod" .!= ""
            <*> o .: "archivePrefix"
            <*> o .: "filePaths"
            <*> o .:? "fileExcludes" .!= []
            <*> do mKeepTime <- pruneObj .:? "keepAllWithin"
                   case mKeepTime of
                     Nothing -> pure ""
                     Just keepTime ->
                       if keepTime == "" ||
                          T.last keepTime `elem` ['H','d','w','m','y']
                       then pure keepTime
                       else fail ("Unknown time interval: " ++ T.unpack keepTime)
            <*> pruneObj .:? "keepIntervals" .!= Interval 0 0 0 0 0

newtype Verbosity = Verbosity
  { _stdoutVerbosity     :: Bool
      -- ^ If 'False', suppress output to stdout.
  }

iVbBool :: Iso' Verbosity Bool
iVbBool = iso _stdoutVerbosity Verbosity

instance J.FromJSON Verbosity where
  parseJSON = J.withBool "Verbosity" $
    pure . Verbosity

data Configuration = Configuration
  { _activeKeywords      :: [T.Text]
      -- ^ List of keywords to search for such that if they exist, the
      -- connection is assumed to be active.
  , _unmeteredConnNames  :: [T.Text]
      -- ^ List of fragments of unmetered connection names to match against, in
      -- lower case. Match is not case sensitive.
  , _osxNotifications    :: Bool
      -- ^ If true, display notifications in notification center. OS X only.
  , _setVerbosity        :: Verbosity
      -- ^ Controls whether to print to stdout.
  , _borgBinPath         :: T.Text
      -- ^ Full path of borg executable.
  , _archiveManifest     :: [Archive]
      -- ^ List of archives to perform backup.
  , _postBackupCmdS      :: [(T.Text, [T.Text])]
      -- ^ List of (Command, Args) to execute after backup is done.
  }

activeKeywords :: Lens' Configuration [T.Text]
activeKeywords f t =
  (\p' -> t {_activeKeywords = p'}) <$> f (_activeKeywords t)

unmeteredConnNames :: Lens' Configuration [T.Text]
unmeteredConnNames f t =
  (\p' -> t {_unmeteredConnNames = p'}) <$> f (_unmeteredConnNames t)

osxNotifications :: Lens' Configuration Bool
osxNotifications f t =
  (\p' -> t {_osxNotifications = p'}) <$> f (_osxNotifications t)

setVerbosity :: Lens' Configuration Verbosity
setVerbosity f t =
  (\p' -> t {_setVerbosity = p'}) <$> f (_setVerbosity t)

borgBinPath :: Lens' Configuration T.Text
borgBinPath f t =
  (\p' -> t {_borgBinPath = p'}) <$> f (_borgBinPath t)

archiveManifest :: Lens' Configuration [Archive]
archiveManifest f t =
  (\p' -> t {_archiveManifest = p'}) <$> f (_archiveManifest t)

postBackupCmdS :: Lens' Configuration [(T.Text, [T.Text])]
postBackupCmdS f t =
  (\p' -> t {_postBackupCmdS = p'}) <$> f (_postBackupCmdS t)

instance J.FromJSON Configuration where
  parseJSON = J.withObject "Configuration" $ \o -> do
    postBupObj <- o .:? "postBackupCommands" .!= mempty
    Configuration <$> o .: "activeKeywords"
                  <*> o .: "unmeteredConnNames"
                  <*> o .:? "osxNotifications" .!= False
                  <*> o .:? "stdoutVerbosity" .!= Verbosity False
                  <*> o .: "borgBinPath"
                  <*> o .: "archiveManifest"
                  <*> (mapM (J.withObject "CommandArgsTuple" $ \c ->
                              (,) <$> c .: "cmd"
                                  <*> c .:? "args" .!= mempty
                        ) (V.toList postBupObj))
