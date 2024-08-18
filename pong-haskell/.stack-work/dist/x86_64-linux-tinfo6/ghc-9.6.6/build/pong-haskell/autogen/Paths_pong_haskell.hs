{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_pong_haskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/bin"
libdir     = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/lib/x86_64-linux-ghc-9.6.6/pong-haskell-0.1.0.0-AaIy8A4tAyUG8RxeObSf50-pong-haskell"
dynlibdir  = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/share/x86_64-linux-ghc-9.6.6/pong-haskell-0.1.0.0"
libexecdir = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/libexec/x86_64-linux-ghc-9.6.6/pong-haskell-0.1.0.0"
sysconfdir = "/home/vaz/pong-haskell/.stack-work/install/x86_64-linux-tinfo6/e05ce2558839c4a3f53ce02a22fe0fa9bcb6ed9e5bbd774689801a23b6e4abe2/9.6.6/etc"

getBinDir     = catchIO (getEnv "pong_haskell_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "pong_haskell_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "pong_haskell_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "pong_haskell_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pong_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pong_haskell_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
