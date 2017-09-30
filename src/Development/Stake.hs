{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.String (fromString)
import Development.Shake
import Development.Shake.Classes
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Program
import Development.Stake.Stackage
import Development.Stake.Witness
import Distribution.Package
import Distribution.Text (simpleParse)

main = runStake $ \(cmd:args) -> do
    downloadCabalPackageRule
    buildPlanRules
    buildPackageRules
    let runClean pat = action $ removeFilesAfter stakeDir [pat]
    case cmd of
        "clean" -> runClean "build"
        "clean-all" -> runClean ""
        "build"
            | planName:pkgNames <- args
                -> action $ do
                        plan <- askWitness $ ReadPlan (PlanName planName)
                        askBuiltPackages plan $ map PackageName pkgNames
        _ -> error $ "Unknown invocation: " ++ show (cmd, args)
