module Development.Stake.Package
    ( getPackageSourceDir
    , configurePackage
    , parseCabalFileInDir
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

getPackageSourceDir :: PackageIdentifier -> Action Artifact
getPackageSourceDir pkg = do
    tarball <- downloadCabalPackage pkg
    runCommand (output outDir)
        $ prog "tar" ["-xzf", relPath tarball, "-C", outPath (takeDirectory outDir)]
        <> input tarball
  where
    outDir = "package/raw" </> display pkg

configurePackage :: BuildPlan -> Artifact -> Action (PackageDescription, Artifact)
configurePackage plan packageSourceDir = do
    gdesc <- parseCabalFileInDir packageSourceDir
    let desc = flattenToDefaultFlags plan gdesc
    let name = display (packageName desc)
    case buildType desc of
        Just Configure -> do
            let configuredDir = "package/configured" </> name
            configuredPackage <- runCommand (output configuredDir)
                $ copyArtifact packageSourceDir configuredDir
                <> withCwd (outPath configuredDir) (progTemp (outPath $ configuredDir </> "configure") [])
            let buildInfoFile = configuredPackage />
                                    (name <.> "buildinfo")
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

parseCabalFileInDir :: Artifact -> Action GenericPackageDescription
parseCabalFileInDir dir = do
    cabalFile <- findCabalFile dir
    cabalContents <- readArtifact cabalFile
    -- TODO: better error message when parse fails; and maybe warnings too?
    case parsePackageDescription cabalContents of
        ParseFailed err -> error $ show err
        ParseOk _ pkg -> return pkg

findCabalFile :: Artifact -> Action Artifact
findCabalFile dir = do
    cabalFiles <- matchArtifactGlob dir "*.cabal"
    case cabalFiles of
        [f] -> return f
        [] -> error $ "No *.cabal files found in " ++ show dir
        _ -> error $ "Multiple *.cabal files found: " ++ show cabalFiles

flattenToDefaultFlags
    :: BuildPlan -> GenericPackageDescription -> PackageDescription
flattenToDefaultFlags plan gdesc = let
    desc0 = packageDescription gdesc
    flags = HM.fromList [(flagName f, flagDefault f)
                        | f <- genPackageFlags gdesc
                        ]
    in desc0
        -- TODO: Nothing vs Nothing?
        { library = resolve plan flags <$> condLibrary gdesc
        , executables = map (\(n, e) -> (resolve plan flags e) { exeName = n })
                            $ condExecutables gdesc
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
