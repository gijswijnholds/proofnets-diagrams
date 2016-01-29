module Paths_PN2Diagram (
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

bindir     = "/Users/gijs/Library/Haskell/bin"
libdir     = "/Users/gijs/Library/Haskell/ghc-7.10.3-x86_64/lib/PN2Diagram-0.1.0.0"
datadir    = "/Users/gijs/Library/Haskell/share/ghc-7.10.3-x86_64/PN2Diagram-0.1.0.0"
libexecdir = "/Users/gijs/Library/Haskell/libexec"
sysconfdir = "/Users/gijs/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PN2Diagram_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PN2Diagram_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PN2Diagram_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PN2Diagram_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PN2Diagram_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
