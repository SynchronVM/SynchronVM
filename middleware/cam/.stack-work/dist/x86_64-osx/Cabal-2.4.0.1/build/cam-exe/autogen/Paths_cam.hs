{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cam (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/bin"
libdir     = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/lib/x86_64-osx-ghc-8.6.4/cam-0.1.0.0-I6vgyh2CLXR2rXgZ5sjGbQ-cam-exe"
dynlibdir  = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/lib/x86_64-osx-ghc-8.6.4"
datadir    = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/share/x86_64-osx-ghc-8.6.4/cam-0.1.0.0"
libexecdir = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/libexec/x86_64-osx-ghc-8.6.4/cam-0.1.0.0"
sysconfdir = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/.stack-work/install/x86_64-osx/107948f4be285d1b192970ea76efbce415b09a2ade64e500028146c3e21b35c7/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cam_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cam_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cam_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cam_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cam_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cam_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
