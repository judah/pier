{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Package
    ( downloadCabalPackageRule
    ) where

import Data.Version (showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Core
import Distribution.Package
import GHC.Generics (Generic(..))
import Development.Stake.Stackage ()


newtype DownloadedPackage = DownloadedPackage PackageId
    deriving (Show, Typeable, Eq, Binary, Generic, NFData)

type instance RuleResult DownloadedPackage = FilePath

instance Hashable DownloadedPackage where

downloadCabalPackageRule :: Rules ()
-- TODO: avoid clashes?
downloadCabalPackageRule = "downloads/hackage/*/*.cabal" #> \f (n:_) -> do
    let tarPath = takeDirectory f <.> "tar.gz"
    let outputDirParent = takeDirectory $ takeDirectory f
    createParentIfMissing tarPath
    cmd_ "curl"
        (hackageUrl </> "package" </> n </> n <.> "tar.gz")
        ["-Ss", "-o", tarPath]
    cmd_ "tar" ["-xzf", tarPath, "-C", outputDirParent]

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org"
