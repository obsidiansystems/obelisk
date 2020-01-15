{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Command.Preprocessor where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Distribution.Compiler
import Language.Haskell.Extension
import System.Environment
import System.IO
import System.FilePath

import Obelisk.App
import Obelisk.Command.Run

main :: IO ()
main = do
  -- The command line argumens are passed in via ghci.
  -- ghci dictates the first three options and meanings
  -- TODO: perform some validation and print a nice error/help message, if we don't see something plausible.
  -- We do have control over the remaining arguments, but they must be the same for all files.
  -- We assume all the remaining arguments passed in are paths to cabal or hpack package specifications.

  args@(origPath:inPath:outPath:packagePaths) <- getArgs

  putStr "--------------------------------------------------------------------------------\n"
  print args
  putStr "--------------------------------------------------------------------------------\n"

  -- Thus we must select among the packagePaths for the file we are going to parse.

  let packageDirs = sortOn (negate . length) $ map (splitPath . normalise) packagePaths
      origDir = splitPath $ normalise $ origPath
      matches = [ d | d@(_:_) <- packageDirs, takeWhile hasTrailingPathSeperator d `isPrefixOf` origDir ]

  -- So the first element of matches is going to be the deepest path to a package spec that contains
  -- our file as a subdirectory.

  let hPutTextBuilder outFile = BL.hPutBuilder outFile . TL.encodeUtf8Builder . TL.toLazyText
  case matches of
    [] -> hPutTextBuilder (lineNumberPragma origPath) -- TODO: probably should produce a warning
    (packagePath:_) -> do
       runObelisk (ObeliskT mempty) (parseCabalPackage packagePath) >>= \case
         Just packageInfo -> do
           hPutTextBuilder (generateHeader origPath packageInfo)

  BL.readFile inPath >>= BL.hPut outFile
  hClose outFile


 -- I'm pretty sure there's a certain amount of oversimplification in CabalPackageInfo, so I doubt this is fully robust.

generateHeader :: FilePath -> CabalPackageInfo -> TL.Builder
generateHeader origPath packageInfo =
    hsExtensions <> ghcOptions <> lineNumberPragma origPath
  where
    hsExtensions =
      if not (null extList)
      then TL.fromText "{-# LANGUAGE "
        <> mconcat (intersperse (TL.fromText ", ") extList)
        <> TL.fromText " #-}\n"
      else mempty
    extList = concatMap showExt (_cabalPackageInfo_defaultExtensions packageInfo)
    showExt = \case
      EnableExtension ext -> [TL.fromString (show ext)]
      DisableExtension _ -> []
      UnknownExtension ext -> [TL.fromString ext]

    ghcOptions =
      if not (null optList)
      then TL.fromText "{-# OPTIONS_GHC " <>
        <> mconcat (intersperse (TL.fromText " ") optList)
        <> TL.fromText " #-}\n"
      else mempty
    optList = map TL.fromString $ fromMaybe [] $ lookup GHC (_cabalPackageInfo_compilerOptions packageInfo)

lineNumberPragma :: FilePath -> TL.Builder
lineNumberPragma origPath =
  TL.fromText "{-# LINE 1 \"" <> TL.fromString origPath <> TL.fromText "\" #-}\n"
