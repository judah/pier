{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Package
    ( unpackedCabalPackageDir
    , getPackageDescription
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Text (display)
import Distribution.System (buildOS, buildArch)
import Distribution.Version (withinRange)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Compiler

import Development.Stake.Command
import Development.Stake.Download
import Development.Stake.Stackage


downloadCabalPackage :: PackageIdentifier -> Action Artifact
downloadCabalPackage pkg = do
    let n = display pkg
    askDownload Download
        { downloadFilePrefix = "hackage"
        , downloadName = n <.> "tar.gz"
        , downloadUrlPrefix = "https://hackage.haskell.org/package" </> n
        }

unpackedCabalPackageDir :: BuildPlan -> PackageIdentifier -> Action (PackageDescription, Artifact)
unpackedCabalPackageDir plan pkg = do
    tarball <- downloadCabalPackage pkg
    packageSourceDir <- runCommand (output outDir)
        $ prog "tar" ["-xzf", relPath tarball, "-C", takeDirectory outDir]
        <> input tarball
    desc <- getPackageDescription plan (pkgName pkg) packageSourceDir
    case buildType desc of
        Just Configure -> do
            let configuredDir = "package/configured" </> display pkg
            configuredPackage <- runCommand (output configuredDir)
                $ copyArtifact packageSourceDir configuredDir
                <> withCwd configuredDir (progTemp (configuredDir </> "configure") [])
            let buildInfoFile = configuredPackage />
                                    (unPackageName (pkgName pkg) <.> "buildinfo")
            buildInfoExists <- doesArtifactExist buildInfoFile
            desc' <- if buildInfoExists
                then do
                    hookedBIParse <- parseHookedBuildInfo <$> readArtifact buildInfoFile
                    case hookedBIParse of
                        ParseFailed e -> error $ "Error reading buildinfo " ++ show buildInfoFile
                                                    ++ ": " ++ show e
                        ParseOk _ hookedBI -> return $ updatePackageDescription hookedBI desc
                else return desc
            return (desc', configuredPackage)
        -- Best effort: ignore custom setup scripts.
        _ -> return (desc, packageSourceDir)
  where
    outDir = "package/raw" </> display pkg

getPackageDescription :: BuildPlan -> PackageName -> Artifact -> Action PackageDescription
getPackageDescription plan pkg packageSourceDir = do
    -- TODO: better error message when parse fails; and maybe warnings too?
    cabalContents <- readArtifact $ packageSourceDir
                                        /> (display pkg <.> "cabal")
    gdesc <- case parsePackageDescription cabalContents of
                    ParseFailed err -> error $ show err
                    ParseOk _ x -> return x
    return $ flattenToDefaultFlags plan gdesc

flattenToDefaultFlags
    :: BuildPlan -> GenericPackageDescription -> PackageDescription
flattenToDefaultFlags plan gdesc = let
    desc0 = packageDescription gdesc
    flags = HM.fromList [(flagName f, flagDefault f)
                        | f <- genPackageFlags gdesc
                        ]
    in desc0 {
        -- TODO: Nothing vs Nothing?
        library = fmap (resolve plan flags) $ condLibrary gdesc
       }

resolve
    :: Semigroup a
    => BuildPlan
    -> HM.HashMap FlagName Bool
    -> CondTree ConfVar [Dependency] a
    -> a
resolve plan flags node
    = sconcat
        $ condTreeData node :|
        [ resolve plan flags t
        | (cond,ifTrue,ifFalse) <- condTreeComponents node
        , Just t <- [if isTrue plan flags cond
                        then Just ifTrue
                        else ifFalse]]

isTrue :: BuildPlan -> HM.HashMap FlagName Bool -> Condition ConfVar -> Bool
isTrue plan flags = loop
  where
    loop (Var (Flag f))
        | Just x <- HM.lookup f flags = x
        | otherwise = error $ "Unknown flag: " ++ show f
    loop (Var (Impl GHC range)) = withinRange (ghcVersion plan) range
    loop (Var (Impl _ _)) = False
    loop (Var (OS os)) = os == buildOS
    loop (Var (Arch arch)) = arch == buildArch
    loop (Lit x) = x
    loop (CNot x) = not $ loop x
    loop (COr x y) = loop x || loop y
    loop (CAnd x y) = loop x && loop y
