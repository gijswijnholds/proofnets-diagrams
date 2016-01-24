module Paths_PN2Diagram (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/gijs/.cabal/bin"
libdir     = "/home/gijs/.cabal/lib/PN2Diagram-0.1.0.0/ghc-7.6.3"
datadir    = "/home/gijs/.cabal/share/PN2Diagram-0.1.0.0"
libexecdir = "/home/gijs/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "PN2Diagram_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PN2Diagram_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PN2Diagram_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PN2Diagram_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
