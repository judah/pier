{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Package where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Version (showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Core
import Development.Stake.Witness
import Distribution.Package
import Distribution.Version
import GHC.Generics (Generic(..))

import Development.Stake.Stackage

packageIdString :: PackageId -> String
packageIdString p = unPackageName (packageName p)
                                ++ "-"
                                ++ showVersion (packageVersion p)


newtype DownloadedPackage = DownloadedPackage PackageId
    deriving (Show, Typeable, Eq, Binary, Generic, NFData)

type instance RuleResult DownloadedPackage = FilePath

instance Hashable DownloadedPackage where

downloadCabalPackageRule :: Rules ()
-- TODO: avoid clashes?
downloadCabalPackageRule = "downloads/hackage/*/*.cabal" #> \f (n:_) -> do
    let downloadsDir = artifact $ "downloads/hackage"
    let outDir = downloadsDir </> n
    let tarPath = outDir <.> "tar.gz"
    createParentIfMissing tarPath
    createDirectoryIfMissing' downloadsDir
    cmd_ "curl"
        (hackageUrl </> "package" </> n </> n <.> "tar.gz")
        ["-Ss", "-o", tarPath]
    cmd_ "tar" ["-xzf", tarPath, "-C", downloadsDir]

hackageUrl = "http://hackage.haskell.org"

cabalPlanRules :: BuildPlan -> Rules ()
cabalPlanRules plan = mapM_ mkPhony $ HM.toList $ packageVersions plan
  where
    mkPhony :: (T.Text,Version) -> Rules ()
    mkPhony (n,v) = phony n' $ need [artifact $ "downloads/hackage" </>
                                packageIdString (PackageIdentifier
                                                    (PackageName n') v)
                                </> n' <.> "cabal"]
      where n' = T.unpack n

-- OK, the cabal download itself is based on the .cabal file
-- For individual files, like the Haskell modules: first need
-- the .cabal file, then look for the generated file (if any).
