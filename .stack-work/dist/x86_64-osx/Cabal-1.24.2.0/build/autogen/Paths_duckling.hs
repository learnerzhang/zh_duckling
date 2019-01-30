{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_duckling (
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
version = Version [0,1,6,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/bin"
libdir     = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2/duckling-0.1.6.1-FuuoxNkK08bIA0dGJuhBV1"
dynlibdir  = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/share/x86_64-osx-ghc-8.0.2/duckling-0.1.6.1"
libexecdir = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/libexec"
sysconfdir = "/Users/zhangzhen/gitRepository/zh_duckling/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "duckling_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "duckling_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "duckling_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "duckling_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "duckling_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "duckling_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
