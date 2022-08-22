{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Command.Preprocessor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BU
import Data.Foldable (for_)
import Data.List (intersperse, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import Distribution.Compiler (CompilerFlavor (..), perCompilerFlavorToList)
import Language.Haskell.Extension (Extension (..), Language(..))
import System.Directory (canonicalizePath)
import System.IO (IOMode (..), hPutStrLn, stderr, withFile)
import System.FilePath (hasTrailingPathSeparator, joinPath, splitPath)
import Control.Lens ((<&>))

import Obelisk.Command.Run (CabalPackageInfo (..), parseCabalPackage')

-- | This code is intended to be executed via ghci's -pgmF preprocessor option.
-- The command line arguments are passed in via ghc, which dictates the first three options and meanings
-- In order for this code to execute, origPath must contain either a '.' character or a '/' character.
-- (This is to avoid the possibility of the command line syntax conflicting with another ob command)
-- We do have control over the remaining arguments, but they must be the same for all files.
-- Thus, the fourth command line argument must be "apply-packages",  which has already been handled.
-- We assume all the remaining arguments passed in are paths to cabal or hpack package specifications.
-- Thus we must select among the packagePaths for the file we are going to parse.
applyPackages :: FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
applyPackages origPath inPath outPath packagePaths' = do
  origPathCanonical <- canonicalizePath origPath
  packagePaths <- traverse canonicalizePath packagePaths'

  let
    takeDirs = takeWhile hasTrailingPathSeparator
    packageDirs = sortOn (negate . length . takeDirs) $ map splitPath packagePaths
    origDir = splitPath origPathCanonical
    matches = [ joinPath d | d <- packageDirs, takeDirs d `isPrefixOf` origDir ]

  -- The first element of matches is going to be the deepest path to a package spec that contains
  -- our file as a subdirectory.
  packageInfo' <- case matches of
    [] -> do
      hPutStrLn stderr $ "Error: Unable to find cabal information for " <> origPath <> "; Skipping preprocessor."
      pure Nothing
    packagePath:_ -> parseCabalPackage' packagePath >>= \case
      Left err -> do
        hPutStrLn stderr $ "Error: Unable to parse cabal package " <> packagePath <> "; Skipping preprocessor on " <> origPath <> ". Error: " <> show err
        pure Nothing
      Right (Just (_, packageInfo)) -> pure $ Just packageInfo
      Right Nothing -> pure Nothing

  writeOutput packageInfo' inPath outPath

writeOutput :: Maybe CabalPackageInfo -> FilePath -> FilePath -> IO ()
writeOutput packageInfo' origPath outPath = withFile outPath WriteMode $ \hOut -> do
  for_ packageInfo' $ \packageInfo ->
    case generateHeader origPath packageInfo of
      Left e -> hPutStrLn stderr (prettyGenHeaderError origPath e)
      Right header -> hPutTextBuilder hOut header
  BL.readFile origPath >>= BL.hPut hOut
  where
    hPutTextBuilder h = BU.hPutBuilder h . TL.encodeUtf8Builder . TL.toLazyText

-- | Represents an error which may happen when turning a
-- 'CabalPackageInfo' into a set of GHC pragmas.
data GenHeaderError
  = GenHeaderError_UnknownLanguage String
  -- ^ An invalid @default-language@ field was specified.
  | GenHeaderError_UnknownExtension String
  -- ^ An invalid value was present in the @default-extensions@ field.

-- | Turn a 'GenHeaderError' to a string suitable for display to the user.
prettyGenHeaderError :: String -> GenHeaderError -> String
prettyGenHeaderError origPath =
  \case
    GenHeaderError_UnknownExtension e -> "Error: Unknown default-extension " <> e <> "; Skipping preprocessor on " <> origPath <> "."
    GenHeaderError_UnknownLanguage e -> "Error: Unknown default-language " <> e <> "; Skipping preprocessor on " <> origPath <> "."

-- | Turn a parsed 'CabalPackageInfo' into a set of GHC pragmas which
-- reproduce the same settings.
--
-- NOTE: We cannot restrict the package set by adding '-package' flags
-- to OPTIONS_GHC, because GHC rejects them there.  It seems that we
-- won't be able to properly handle that situation until GHC itself
-- supports loading multiple packages officially in GHCi
generateHeader :: FilePath -> CabalPackageInfo -> Either GenHeaderError TL.Builder
generateHeader origPath packageInfo =
    fmap (\e -> e <> ghcOptions <> lineNumberPragma origPath) hsExtensions
  where
    hsExtensions :: Either GenHeaderError TL.Builder
    hsExtensions = extList <&> \exts ->
      if null exts
        then mempty
        else pragma $ TL.fromText "LANGUAGE " <> mconcat (intersperse (TL.fromText ", ") exts)

    extList :: Either GenHeaderError [TL.Builder]
    extList = addDefaultLanguage =<< traverse showExt (_cabalPackageInfo_defaultExtensions packageInfo)

    addDefaultLanguage :: [TL.Builder] -> Either GenHeaderError [TL.Builder]
    addDefaultLanguage exts =
      case _cabalPackageInfo_defaultLanguage packageInfo of
        Nothing -> pure exts
        Just x -> case x of
          UnknownLanguage ext -> Left (GenHeaderError_UnknownExtension ext)
          ext -> pure $ TL.fromString (show ext):exts

    showExt :: Extension -> Either GenHeaderError TL.Builder
    showExt = \case
      EnableExtension ext -> pure $ TL.fromString (show ext)
      DisableExtension ext -> pure $ "No" <> TL.fromString (show ext)
      UnknownExtension ext -> Left (GenHeaderError_UnknownExtension ext)

    ghcOptions =
      if null optList
        then mempty
        else pragma $
          TL.fromText "OPTIONS_GHC " <> mconcat (intersperse (TL.fromText " ") (map TL.fromString optList))
    ghcOptList
      = filter (not . isPrefixOf "-O")
      $ fromMaybe []
      $ lookup GHC
      $ perCompilerFlavorToList
      $ _cabalPackageInfo_compilerOptions packageInfo
    optList = _cabalPackageInfo_cppOptions packageInfo <> ghcOptList

lineNumberPragma :: FilePath -> TL.Builder
lineNumberPragma origPath = pragma $ TL.fromText "LINE 1 " <> quoted '"' (TL.fromString origPath)

pragma :: TL.Builder -> TL.Builder
pragma x = TL.fromText "{-# " <> x <> TL.fromText " #-}\n"

quoted :: Char -> TL.Builder -> TL.Builder
quoted char x = TL.singleton char <> x <> TL.singleton char
