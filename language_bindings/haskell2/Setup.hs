-- Setup.hs is generated automatically: do not edit

module Main (main) where

import Control.Monad
import System.Process
import System.Exit
import Data.Version
import Distribution.Simple
import Distribution.Simple.Setup 
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Verbosity
import Distribution.Simple.Configure 
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  buildHook = myBuildHook,
  preClean = disabledCmd,
  preSDist = disabledCmd,
  preHaddock = disabledCmd
}

myBuildHook pd lbi mbuhks bflg = do
  let (PackageName pkg) = pkgName $ package pd
      ver = showVersion $ pkgVersion $ package pd
      loc = buildDir lbi
      verb = buildVerbose bflg
      mkcmd = "make build PKG=" ++ pkg ++ " VER=" ++ ver ++ " LOC=" ++ loc
  when (verb > silent) $ putStrLn $ "Running command:" ++ mkcmd
  ec <- runCommand mkcmd >>= waitForProcess
  when (verb > silent) $ putStrLn $ "Command exit code: " ++ (show ec)
  return ()

disabledCmd :: Args -> a -> IO HookedBuildInfo

disabledCmd _ _ = do
  putStrLn "This command is disabled for this package"
  exitWith (ExitFailure 1)
  return emptyHookedBuildInfo

