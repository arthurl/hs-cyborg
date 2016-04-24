-- | A module exporting data structures.

module Borg.Data
  (
  -- * Data structures
    Archive(..)
  , Configuration(..)

  -- * Lenses and Prisms
  -- ** Archive
  , repositoryLoc
  , compressionMethod
  , archivePrefix
  , filePaths
  , fileExcludes
  , retentionIntervalLimit

  -- ** Configuration
  , activeKeywords
  , unmeteredConnNames
  , borgBinPath
  , archiveManifest
  ) where

import Control.Lens (Lens')
import qualified Data.Text as T
import qualified Data.Aeson as J (FromJSON, parseJSON, withObject)
import Data.Aeson ((.:), (.:?))

data Archive = Archive
  { _repositoryLoc           :: T.Text
      -- ^ Loc
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
  , _retentionIntervalLimit  :: Maybe ()
  } deriving (Show)

repositoryLoc :: Lens' Archive T.Text
repositoryLoc f t =
  (\p' -> t {_repositoryLoc = p'}) <$> f (_repositoryLoc t)

compressionMethod :: Lens' Archive T.Text
compressionMethod f t =
  (\p' -> t {_compressionMethod = p'}) <$> f (_compressionMethod t)

archivePrefix :: Lens' Archive T.Text
archivePrefix f t =
  (\p' -> t {_archivePrefix = p'}) <$> f (_archivePrefix t)

filePaths :: Lens' Archive [FilePath]
filePaths f t =
  (\p' -> t {_filePaths = p'}) <$> f (_filePaths t)

fileExcludes :: Lens' Archive [T.Text]
fileExcludes f t =
  (\p' -> t {_fileExcludes = p'}) <$> f (_fileExcludes t)

retentionIntervalLimit :: Lens' Archive (Maybe ())
retentionIntervalLimit f t =
  (\p' -> t {_retentionIntervalLimit = p'}) <$> f (_retentionIntervalLimit t)

instance J.FromJSON Archive where
  parseJSON = J.withObject "Archive" $ \o ->
    Archive <$> o .: "repositoryLoc"
            <*> o .: "compressionMethod"
            <*> o .: "archivePrefix"
            <*> o .: "filePaths"
            <*> o .: "fileExcludes"
            <*> o .:? "retentionIntervalLimit"

data Configuration = Configuration
  { _activeKeywords      :: [T.Text]
      -- ^ List of keywords to search for such that if they exist, the
      -- connection is assumed to be active.
  , _unmeteredConnNames  :: [T.Text]
      -- ^ List of fragments of unmetered connection names to match against, in
      -- lower case. Match is not case sensitive.
  , _borgBinPath         :: T.Text
      -- ^ Full path of borg executable.
  , _archiveManifest     :: [Archive]
      -- ^ List of archives to perform backup.
  }

activeKeywords :: Lens' Configuration [T.Text]
activeKeywords f t =
  (\p' -> t {_activeKeywords = p'}) <$> f (_activeKeywords t)

unmeteredConnNames :: Lens' Configuration [T.Text]
unmeteredConnNames f t =
  (\p' -> t {_unmeteredConnNames = p'}) <$> f (_unmeteredConnNames t)

borgBinPath :: Lens' Configuration T.Text
borgBinPath f t =
  (\p' -> t {_borgBinPath = p'}) <$> f (_borgBinPath t)

archiveManifest :: Lens' Configuration [Archive]
archiveManifest f t =
  (\p' -> t {_archiveManifest = p'}) <$> f (_archiveManifest t)

instance J.FromJSON Configuration where
  parseJSON = J.withObject "Configuration" $ \o ->
    Configuration <$> o .: "activeKeywords"
                  <*> o .: "unmeteredConnNames"
                  <*> o .: "borgBinPath"
                  <*> o .: "archiveManifest"
