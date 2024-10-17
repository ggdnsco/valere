module Valere.Snapshot
  ( Entry(..)
  , Snapshot(..)
  , takeSnapshot
  )
where

import Prelude hiding (readFile)

import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Lazy

import Valere.Utils

import Data.HashSet qualified as HashSet

import Data.HashSet (HashSet)
import Data.ByteString.Short (ShortByteString)
import Data.String (IsString(..))
import Data.Digest.Pure.MD5 (MD5Digest, md5, md5DigestBytes)
import Data.Functor ((<&>))
import Data.Hashable (Hashable (hashWithSalt), hash)
import System.FilePath ((</>))

data Entry
  = Directory { path :: !ShortByteString }
  | File { path :: !ShortByteString, digest :: !MD5Digest }
  deriving (Show, Eq)

instance Hashable Entry where
  hashWithSalt salt (Directory path) = hashWithSalt salt path
  hashWithSalt salt (File path digest) =
    hashWithSalt salt path `hashWithSalt` hash (md5DigestBytes digest)

entryFromFilePath :: FileSystem :> es => FilePath -> Eff es Entry
entryFromFilePath filePath = do
  -- TODO(m4xine): Self-explanitory.
  ifM (doesDirectoryExist filePath)
    do pure (Directory $ fromString filePath)
    do ifM (doesFileExist filePath)
        do readFile filePath <&> File (fromString filePath) . md5
        do error filePath

newtype Snapshot = Snapshot (HashSet Entry)
  deriving Show

takeSnapshot
  :: FileSystem :> es
  => FilePath
  -- ^ Filepath to directory
  -> Eff es Snapshot
takeSnapshot = fmap Snapshot . go
  where
    go filePath =
      foldMap (go' . (filePath </>)) =<< listDirectory filePath
    go' filePath = do
      entry <- entryFromFilePath filePath
      subEntries <- doesDirectoryExist filePath >>= \case
        True -> go filePath
        False -> mempty
      pure $ HashSet.singleton entry <> subEntries