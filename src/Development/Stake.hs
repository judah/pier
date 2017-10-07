{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake (main) where

import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Distribution.Package

main :: IO ()
main = runStake $ \(command:args) -> do
    downloadCabalPackageRule
    buildPlanRules
    buildPackageRules
    case command of
        "clean" -> cleanBuild
        "clean-all" -> cleanAll
        "build"
            | planName:pkgNames <- args
                -> action $ do
                        let n = PlanName planName
                        askBuiltPackages n
                            $ map PackageName pkgNames
        _ -> error $ "Unknown invocation: " ++ show (command, args)
