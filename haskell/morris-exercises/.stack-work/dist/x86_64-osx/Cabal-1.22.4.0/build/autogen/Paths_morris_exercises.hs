module Paths_morris_exercises (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/halarnold/haskell/morris-exercises/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/bin"
libdir     = "/Users/halarnold/haskell/morris-exercises/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/lib/x86_64-osx-ghc-7.10.2/morris-exercises-0.1.0.0-BG7fVuhsaya1E5p349G2eB"
datadir    = "/Users/halarnold/haskell/morris-exercises/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/share/x86_64-osx-ghc-7.10.2/morris-exercises-0.1.0.0"
libexecdir = "/Users/halarnold/haskell/morris-exercises/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/libexec"
sysconfdir = "/Users/halarnold/haskell/morris-exercises/.stack-work/install/x86_64-osx/lts-3.4/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morris_exercises_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morris_exercises_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "morris_exercises_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morris_exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morris_exercises_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)