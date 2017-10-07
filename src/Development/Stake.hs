{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake where

import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Development.Stake.Witness
import Distribution.Package

main :: IO ()
main = runStake $ \(command:args) -> do
    downloadCabalPackageRule
    buildPlanRules
    buildPackageRules
    let runClean pat = action $ removeFilesAfter stakeDir [pat]
    case command of
        "clean" -> runClean "build"
        "clean-all" -> runClean ""
        "build"
            | planName:pkgNames <- args
                -> action $ do
                        plan <- askWitness $ ReadPlan (PlanName planName)
                        askBuiltPackages plan $ map PackageName pkgNames
        _ -> error $ "Unknown invocation: " ++ show (command, args)
