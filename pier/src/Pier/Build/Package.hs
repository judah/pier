module Pier.Build.Package
    ( getPackageSourceDir
    , configurePackage
    , parseCabalFileInDir
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Development.Shake
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.System (buildOS, buildArch)
import Distribution.Text (display)
import Distribution.Types.CondTree (CondBranch(..))
import Distribution.Version (withinRange)

import qualified Data.HashMap.Strict as HM

import Pier.Build.Stackage
import Pier.Core.Artifact
import Pier.Core.Download

downloadCabalPackage :: PackageIdentifier -> Action Artifact
downloadCabalPackage pkg = do
    let n = display pkg
    askDownload Download
        { downloadName = n <.> "tar.gz"
        , downloadUrlPrefix = "https://hackage.haskell.org/package/" ++ n
        }

getPackageSourceDir :: PackageIdentifier -> Action Artifact
getPackageSourceDir pkg = do
    tarball <- downloadCabalPackage pkg
    runCommandOutput outDir
        $ message ("Unpacking " ++ display pkg)
        <> prog "tar" ["-xzf", pathIn tarball, "-C", takeDirectory outDir]
        <> input tarball
  where
    outDir = "package/raw" </> display pkg

configurePackage :: BuildPlan -> Flags -> Artifact -> Action (PackageDescription, Artifact)
configurePackage plan flags packageSourceDir = do
    gdesc <- parseCabalFileInDir packageSourceDir
    let desc = flattenToDefaultFlags plan flags gdesc
    let name = display (packageName desc)
    case buildType desc of
        Configure -> do
            let configuredDir = name
            configuredPackage <- runCommandOutput configuredDir
                $ shadow packageSourceDir configuredDir
                <> message ("Configuring " ++ name)
                <> withCwd configuredDir (progTemp (configuredDir </> "configure") [])
            let buildInfoFile = configuredPackage />
                                    (name <.> "buildinfo")
            buildInfoExists <- doesArtifactExist buildInfoFile
            desc' <- if buildInfoExists
                then do
                    hookedBI <- readHookedBuildInfoA buildInfoFile
                    return $ updatePackageDescription hookedBI desc
                else return desc
            return (desc', configuredPackage)
        -- Best effort: ignore custom setup scripts.
        _ -> return (desc, packageSourceDir)

parseCabalFileInDir :: Artifact -> Action GenericPackageDescription
parseCabalFileInDir dir = do
    cabalFile <- findCabalFile dir
    cabalContents <- readArtifactB cabalFile
    -- TODO: better error message when parse fails; and maybe warnings too?
    case runParseResult $ parseGenericPackageDescription cabalContents of
        (_, Right pkg) -> return pkg
        e -> error $ show e ++ "\n" ++ show cabalContents

readHookedBuildInfoA :: Artifact -> Action HookedBuildInfo
readHookedBuildInfoA file = do
    hookedBIParse <- parseHookedBuildInfo <$> readArtifactB file
    case runParseResult hookedBIParse of
        (_,Right hookedBI) -> return hookedBI
        e -> error $ "Error reading buildinfo " ++ show file
                                                    ++ ": " ++ show e

findCabalFile :: Artifact -> Action Artifact
findCabalFile dir = do
    cabalFiles <- matchArtifactGlob dir "*.cabal"
    case cabalFiles of
        [f] -> return (dir /> f)
        [] -> error $ "No *.cabal files found in " ++ show dir
        _ -> error $ "Multiple *.cabal files found: " ++ show cabalFiles

flattenToDefaultFlags
    :: BuildPlan -> Flags -> GenericPackageDescription -> PackageDescription
flattenToDefaultFlags plan planFlags gdesc = let
    desc0 = packageDescription gdesc
    -- Bias towards plan flags (since they override the defaults)
    flags = planFlags `HM.union` HM.fromList [(flagName f, flagDefault f)
                        | f <- genPackageFlags gdesc
                        ]
    in desc0
        -- TODO: Nothing vs Nothing?
        { library = resolve plan flags <$> condLibrary gdesc
        , executables = map (\(n, e) -> (resolve plan flags e) { exeName = n })
                            $ condExecutables gdesc
        , testSuites = map (\(n, s) -> (resolve plan flags s) { testName = n })
                            $ condTestSuites gdesc
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
        | CondBranch cond ifTrue ifFalse
            <- condTreeComponents node
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
