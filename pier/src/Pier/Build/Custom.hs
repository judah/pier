-- Hacks around Happy and Alex's custom setup scripts, which are used to
-- generate the templates they use at runtime.
--
-- TODO: find a more generic solution for this.
module Pier.Build.Custom
    ( collectHappyDataFiles
    , collectAlexDataFiles
    , addDistSourceDirs
    ) where

import Data.Char (isDigit)
import Development.Shake
import Development.Shake.FilePath
import Distribution.PackageDescription
import Distribution.Text (display)

import Pier.Build.Stackage
import Pier.Core.Artifact

-- | Older versions of Happy and Alex were distributed with a "dist" directory
-- (remnant of Cabal) that contained some bootstrapped source files.
-- Add that directory to the hs-source-dirs for every executable in the package.
addDistSourceDirs :: PackageDescription -> PackageDescription
addDistSourceDirs pkg
    = pkg { executables = map addDistToExe
                                $ executables pkg
          }
  where
    addDistToExe e = e {
        buildInfo = (buildInfo e) {
            hsSourceDirs = distPath (display $ exeName e)
                            : hsSourceDirs (buildInfo e)
        }
    }
    distPath name = "dist/build" </> name </> name ++ "-tmp"

collectHappyDataFiles
    :: InstalledGhc -> Artifact -> Action Artifact
collectHappyDataFiles ghc dir = do
    as <- concat <$> sequence
        [ mapM (uncurry $ processTemplate ghc (dir /> "templates/GenericTemplate.hs"))
             templates
        , mapM (uncurry $ processTemplate ghc (dir /> "templates/GLR_Base.hs"))
             glr_base_templates
        , mapM (uncurry $ processTemplate ghc (dir /> "templates/GLR_Lib.hs"))
             glr_templates
        ]
    let files = "data-files"
    runCommandOutput files $
        foldMap (\a -> shadow a $ files </> takeBaseName (pathIn a))
            as
  where
    templates :: [(FilePath,[String])]
    templates = [
      ("HappyTemplate"                      , []),
      ("HappyTemplate-ghc"                  , ["-DHAPPY_GHC"]),
      ("HappyTemplate-coerce"               , ["-DHAPPY_GHC","-DHAPPY_COERCE"]),
      ("HappyTemplate-arrays"               , ["-DHAPPY_ARRAY"]),
      ("HappyTemplate-arrays-ghc"           , ["-DHAPPY_ARRAY","-DHAPPY_GHC"]),
      ("HappyTemplate-arrays-coerce"        , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_COERCE"]),
      ("HappyTemplate-arrays-debug"         , ["-DHAPPY_ARRAY","-DHAPPY_DEBUG"]),
      ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),
      ("HappyTemplate-arrays-coerce-debug"  , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_COERCE","-DHAPPY_DEBUG"])
     ]

    glr_base_templates :: [(FilePath,[String])]
    glr_base_templates = [
      ("GLR_Base"           , [])
     ]

    glr_templates :: [(FilePath,[String])]
    glr_templates = [
      ("GLR_Lib"            , []),
      ("GLR_Lib-ghc"        , ["-DHAPPY_GHC"]),
      ("GLR_Lib-ghc-debug"  , ["-DHAPPY_GHC", "-DHAPPY_DEBUG"])
     ]




collectAlexDataFiles
    :: InstalledGhc -> Artifact -> Action Artifact
collectAlexDataFiles ghc dir =  do
    as <- concat <$> sequence
        [ mapM (uncurry $ processTemplate ghc (dir /> "templates/GenericTemplate.hs"))
             templates
        , mapM (uncurry $ processTemplate ghc (dir /> "templates/wrappers.hs"))
             wrappers
        ]
    let files = "data-files"
    runCommandOutput files $
        foldMap (\a -> shadow a $ files </> takeBaseName (pathIn a))
            as
  where
    templates :: [(FilePath,[String])]
    templates = [
      ("AlexTemplate",           []),
      ("AlexTemplate-ghc",       ["-DALEX_GHC"]),
      ("AlexTemplate-ghc-nopred",["-DALEX_GHC", "-DALEX_NOPRED"]),
      ("AlexTemplate-ghc-debug", ["-DALEX_GHC","-DALEX_DEBUG"]),
      ("AlexTemplate-debug",     ["-DALEX_DEBUG"])
     ]

    wrappers :: [(FilePath,[String])]
    wrappers = [
      ("AlexWrapper-basic", ["-DALEX_BASIC"]),
      ("AlexWrapper-basic-bytestring", ["-DALEX_BASIC_BYTESTRING"]),
      ("AlexWrapper-strict-bytestring", ["-DALEX_STRICT_BYTESTRING"]),
      ("AlexWrapper-posn",  ["-DALEX_POSN"]),
      ("AlexWrapper-posn-bytestring", ["-DALEX_POSN_BYTESTRING"]),
      ("AlexWrapper-monad", ["-DALEX_MONAD"]),
      ("AlexWrapper-monad-bytestring", ["-DALEX_MONAD_BYTESTRING"]),
      ("AlexWrapper-monadUserState", ["-DALEX_MONAD", "-DALEX_MONAD_USER_STATE"]),
      ("AlexWrapper-monadUserState-bytestring", ["-DALEX_MONAD_BYTESTRING", "-DALEX_MONAD_USER_STATE"]),
      ("AlexWrapper-gscan", ["-DALEX_GSCAN"])
     ]

processTemplate
    :: InstalledGhc -> Artifact -> String -> [String] -> Action Artifact
processTemplate ghc baseTemplate outFile args = do
    a <- runCommandOutput outFile
        $ ghcProg ghc
            (["-o", outFile, "-E", "-cpp", pathIn baseTemplate] ++ args)
        <> input baseTemplate
    writeArtifact outFile . unlines . map mungeLinePragma . lines
        =<< readArtifact a


--------------------------------------------------------------------------------
-- Copied from Setup.hs scripts for happy/alex

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
mungeLinePragma :: String -> String
mungeLinePragma line = case symbols line of
    syms | Just prag <- getLinePrag syms  -> prag
    -- Also convert old-style CVS lines, no idea why we do this...
    ("--":"$":"Id":":":_) -> filter (/='$') line
    (     "$":"Id":":":_) -> filter (/='$') line
    _ -> line
  where
    getLinePrag :: [String] -> Maybe String
    getLinePrag ("#" : n : string : rest)
        | length rest <= 1   -- clang puts an extra field
        , length string >= 2 && head string == '"' && last string == '"'
        , all isDigit n
        = Just $ "{-# LINE " ++ n ++ " " ++ string ++ " #-}"
    getLinePrag _ = Nothing

    symbols :: String -> [String]
    symbols cs = case lex cs of
              (sym, cs'):_ | not (null sym) -> sym : symbols cs'
              _ -> []

