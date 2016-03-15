module Paths_unification (
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

bindir     = "/home/mattox/class/cs421/periods/unification/code/unification/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/bin"
libdir     = "/home/mattox/class/cs421/periods/unification/code/unification/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/lib/x86_64-linux-ghc-7.10.2/unification-0.1.0.0-HBMLX9crGif9QYmanvkJae"
datadir    = "/home/mattox/class/cs421/periods/unification/code/unification/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/share/x86_64-linux-ghc-7.10.2/unification-0.1.0.0"
libexecdir = "/home/mattox/class/cs421/periods/unification/code/unification/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/libexec"
sysconfdir = "/home/mattox/class/cs421/periods/unification/code/unification/.stack-work/install/x86_64-linux/lts-3.17/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unification_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unification_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "unification_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unification_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unification_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
