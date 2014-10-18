module Paths_countdown (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/spencerward/.cabal/bin"
libdir     = "/Users/spencerward/.cabal/lib/x86_64-osx-ghc-7.8.3/countdown-0.1"
datadir    = "/Users/spencerward/.cabal/share/x86_64-osx-ghc-7.8.3/countdown-0.1"
libexecdir = "/Users/spencerward/.cabal/libexec"
sysconfdir = "/Users/spencerward/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "countdown_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "countdown_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "countdown_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "countdown_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "countdown_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
