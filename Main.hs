module Main where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Path
import Stack.Build
import Stack.Build.Cache
import Stack.Build.ConstructPlan
import Stack.Build.Haddock
import Stack.Build.Installed
import Stack.Build.Source
import Stack.Build.Target
import Stack.Config
import Stack.Constants
import Stack.Options
import Stack.PackageDump
import Stack.Setup
import Stack.Types.Build
import Stack.Types.BuildPlan
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.GhcPkgId
import Stack.Types.Package
import Stack.Types.PackageIdentifier
import Stack.Types.PackageName
import Stack.Types.StackT
import Stack.Types.Version
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set

data StackNixPackage
  = StackNixPackage { stackNixPackageName :: PackageName
                    , stackNixPackageVersion :: Version
                    }

stackNixPackageFromTaskType :: TaskType -> Maybe StackNixPackage
stackNixPackageFromTaskType (TTUpstream p _ _) =
  Just $ StackNixPackage (packageName p) (packageVersion p)
stackNixPackageFromTaskType _ =
  Nothing

stackNixPackageDerivation :: StackNixPackage -> String
stackNixPackageDerivation p =
  "(callHackage \"" ++ show (stackNixPackageName p) ++ "\" \"" ++ show (stackNixPackageVersion p) ++ "\" { })"

resolverAttribute :: LoadedResolver -> String
resolverAttribute (ResolverSnapshot (LTS a i)) =
  "lts-" ++ show a ++ "_" ++ show i
resolverAttribute (ResolverCompiler (GhcVersion v)) =
  "ghc" ++ filter (/= '.') (versionString v)
resolverAttribute _ =
  "ghc801"

flagCacheFile :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
              => Installed
              -> m (Path Abs File)
flagCacheFile installed = do
    rel <- parseRelFile $
        case installed of
            Library _ gid -> ghcPkgIdString gid
            Executable ident -> packageIdentifierString ident
    dir <- flagCacheLocal
    return $ dir </> rel

main :: IO ()
main = do
  manager <- newTLSManager
  setEnv platformVariantEnvVar "nix"
  let opts = globalOptsFromMonoid True mempty
  runStackLoggingTGlobal manager opts $ do
    lc <- loadConfig (globalConfigMonoid opts) (globalResolver opts) Nothing
    bconfig <- lcLoadBuildConfig lc (globalCompiler opts)
    envConfig <-runStackTGlobal manager bconfig opts $ setupEnv Nothing
    runStackTGlobal manager envConfig opts $ do
      let boptsCli = defaultBuildOptsCLI
      bopts <- asks (configBuild . getConfig)
      let profiling = boptsLibProfile bopts || boptsExeProfile bopts
      (_, mbp, locals, extraToBuild, sourceMap) <- loadSourceMap NeedTargets boptsCli
      menv <- getMinimalEnvOverride
      baseConfigOpts <- mkBaseConfigOpts boptsCli

      (installedMap0, globalDumpPkgs, _, l) <-
          getInstalled menv
                       GetInstalledOpts
                           { getInstalledProfiling = profiling
                           , getInstalledHaddock   = shouldHaddockDeps bopts }
                       sourceMap

      let installedPackage d = Library (dpPackageIdent d) (dpGhcPkgId d)
          installedPackageEntry d = (packageIdentifierName (dpPackageIdent d), (Snap, installedPackage d))
          installedMap = Map.fromList $ installedPackageEntry <$> globalDumpPkgs

      plan <- withLoadPackage menv $ \loadPackage ->
        constructPlan mbp baseConfigOpts locals extraToBuild mempty loadPackage sourceMap installedMap

      let fromLibrary (Library pid gid) = Just (pid, gid)
          fromLibrary _ = Nothing
          packageIdentifierToGhcId = Map.fromList . mapMaybe (fromLibrary . snd) $ Map.elems installedMap0
          taskPackageOpts (TaskConfigOpts missing getOpts) = getOpts . Map.intersection packageIdentifierToGhcId $ Map.fromSet (const ()) missing
          localInstalledPackages = Map.fromList $ (\ps -> (installedPackageIdentifier ps, ps)) . installedPackage <$> l
          cache task = ConfigCache (taskPackageOpts $ taskConfigOpts task) (Set.fromList . Map.elems $ taskPresent task) Set.empty False
          writePretendFlags task = traverse_ (flip writeFlagCache $ cache task) $ Map.lookup (taskProvides task) localInstalledPackages

      traverse_ writePretendFlags $ planTasks plan

      let packageNixPackages = catMaybes . fmap (stackNixPackageFromTaskType . taskType) . Map.elems $ planTasks plan

      liftIO $ putStrLn "with import <nixpkgs> { };"
      liftIO $ putStrLn "runCommand \"true\" {"
      liftIO . putStrLn $ "  buildInputs = [ stack (haskell.packages." ++ resolverAttribute (bcResolver bconfig) ++ ".ghcWithPackages (p: with p; [ " ++ unwords (stackNixPackageDerivation <$> packageNixPackages) ++ " ])) ];"
      liftIO $ putStrLn "  registerStackPackages = ''"
      forM_ packageNixPackages $ \p ->
        liftIO . putStrLn $ "    ghc-pkg describe " ++ show (stackNixPackageName p) ++ " | GHC_PACKAGE_PATH=$(stack path --local-pkg-db) ghc-pkg register --force -"
      liftIO $ putStrLn "  '';"
      liftIO $ putStrLn "} \"\""
