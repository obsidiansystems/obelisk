{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Command.Run where

import Control.Applicative (liftA2)
import Control.Exception (Exception, bracket)
import Control.Monad
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Either
import Data.Foldable (for_)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult (runParseResult)
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic
import qualified Graphics.Vty as V
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import qualified Hpack.Yaml as Hpack
import Language.Haskell.Extension
import Network.Socket hiding (Debug)
import Reflex.Process (Process(..))
import Reflex.Process.GHCi (ghciWatch, Ghci(..))
import Reflex.Vty hiding (mapMaybe)
import Reflex.Vty.GHCi (ghciPanes)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P (proc, terminateProcess)
import System.Which (staticWhich)



import Obelisk.App (MonadObelisk, ObeliskT)
import Obelisk.CliApp (Severity (..) , callCommand, failWith, putLog, proc, readProcessAndLogStderr)
import Obelisk.Command.Project (inProjectShell, withProjectRoot)

data CabalPackageInfo = CabalPackageInfo
  { _cabalPackageInfo_packageRoot :: FilePath
  , _cabalPackageInfo_sourceDirs :: NE.NonEmpty FilePath
    -- ^ List of hs src dirs of the library component
  , _cabalPackageInfo_defaultExtensions :: [Extension]
    -- ^ List of globally enable extensions of the library component
  , _cabalPackageInfo_defaultLanguage :: Maybe Language
    -- ^ List of globally set languages of the library component
  , _cabalPackageInfo_compilerOptions :: [(CompilerFlavor, [String])]
    -- ^ List of compiler-specific options (e.g., the "ghc-options" field of the cabal file)
  }

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: ObeliskT IO ()
run = do
  let nixPath = $(staticWhich "nix")
      nixBuildPath = $(staticWhich "nix-build")
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    freePort <- getFreePort
    assets <- withProjectRoot "." $ \root -> do
      let importableRoot = if "/" `isInfixOf` root
            then root
            else "./" <> root
      isDerivation <- readProcessAndLogStderr Debug $
        proc nixPath
          [ "eval"
          , "-f"
          , root
          , "(let a = import " <> importableRoot <> " {}; in toString (a.reflex.nixpkgs.lib.isDerivation a.passthru.staticFilesImpure))"
          , "--raw"
          -- `--raw` is not available with old nix-instantiate. It drops quotation
          -- marks and trailing newline, so is very convenient for shelling out.
          ]
      -- Check whether the impure static files are a derivation (and so must be built)
      if isDerivation == "1"
        then fmap T.strip $ readProcessAndLogStderr Debug $ -- Strip whitespace here because nix-build has no --raw option
          proc nixBuildPath
            [ "--no-out-link"
            , "-E", "(import " <> importableRoot <> "{}).passthru.staticFilesImpure"
            ]
        else readProcessAndLogStderr Debug $
          proc nixPath ["eval", "-f", root, "passthru.staticFilesImpure", "--raw"]
    putLog Debug $ "Assets impurely loaded from: " <> assets
    runReflexGhci dotGhciPath $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(runServeAsset " ++ show assets ++ ")"
      , "Backend.backend"
      , "Frontend.frontend"
      ]

runRepl :: MonadObelisk m => m ()
runRepl = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    runGhciRepl dotGhciPath

runWatch :: MonadObelisk m => m ()
runWatch = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> runGhcid dotGhciPath Nothing

data GuessPackageFileError = GuessPackageFileError_Ambiguous [FilePath] | GuessPackageFileError_NotFound
  deriving (Eq, Ord, Show)
instance Exception GuessPackageFileError

-- | Given a directory, try to guess what the appropriate @.cabal@ or @package.yaml@ file is for the package.
guessCabalPackageFile
  :: MonadIO m
  => FilePath -- ^ Directory to search for cabal package
  -> m (Either GuessPackageFileError (Either FilePath FilePath))
  -- ^ 'Right' 'Left' for hpack package, 'Right' 'Right' for cabal package; 'FilePath' is relative to given directory.
guessCabalPackageFile dir = do
  candidates <- liftIO $
        filterM (doesFileExist . (dir </>))
    =<< filter (liftA2 (||) (== Hpack.packageConfig) (".cabal" `L.isSuffixOf`))
    <$> listDirectory dir
  pure $ case L.partition (== Hpack.packageConfig) candidates of
    ([hpack], _) -> Right $ Left hpack
    ([], [cabal]) -> Right $ Right cabal
    ([], []) -> Left GuessPackageFileError_NotFound
    (hpacks, cabals) -> Left $ GuessPackageFileError_Ambiguous $ hpacks <> cabals

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: Applicative f => f [FilePath]
getLocalPkgs = pure ["backend", "common", "frontend"]

parseCabalPackage
  :: (MonadObelisk m)
  => FilePath -- ^ package directory
  -> m (Maybe CabalPackageInfo)
parseCabalPackage dir = either ((Nothing <$) . putLog Error) (pure . Just) <=< runExceptT $ do
  (cabalContents, packageFile) <- guessCabalPackageFile dir >>= \case
    Left GuessPackageFileError_NotFound -> throwError $ "No .cabal or package.yaml file found in " <> T.pack dir
    Left (GuessPackageFileError_Ambiguous _) -> throwError $ "Unable to determine which .cabal file to use in " <> T.pack dir
    Right (Right cabalFileName) -> let file = dir </> cabalFileName in (, file) <$> liftIO (readUTF8File file)
    Right (Left hpackFileName) -> do
      let
        file = dir </> hpackFileName
        decodeOptions = Hpack.DecodeOptions (Hpack.ProgramName "ob") file Nothing Hpack.decodeYaml
      liftIO (Hpack.readPackageConfig decodeOptions) >>= \case
        Left err -> throwError $ T.pack $ "Failed to parse " <> file <> ": " <> err
        Right (Hpack.DecodeResult hpackPackage _ _ _) -> pure (Hpack.renderPackage [] hpackPackage, file)

  let (warnings, result) = runParseResult $ parseGenericPackageDescription $
        toUTF8BS cabalContents
  for_ warnings $ putLog Warning . T.pack . show
  case condLibrary <$> result of
    Right (Just condLib) -> do
      let (_, lib) = simplifyCondTree (const $ pure True) condLib
      pure $ CabalPackageInfo
        { _cabalPackageInfo_packageRoot = dir
        , _cabalPackageInfo_sourceDirs =
            fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
        , _cabalPackageInfo_defaultExtensions =
            defaultExtensions $ libBuildInfo lib
        , _cabalPackageInfo_defaultLanguage =
            defaultLanguage $ libBuildInfo lib
        , _cabalPackageInfo_compilerOptions = options $ libBuildInfo lib
        }
    Right Nothing -> throwError "Haskell package has no library component"
    Left (_, errors) ->
      throwError $ T.pack $ "Failed to parse " <> packageFile <> ":\n" <> unlines (map show errors)

-- | Create ghci configuration to load the given packages
withGhciScript
  :: MonadObelisk m
  => [FilePath] -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporary .ghci
  -> m ()
withGhciScript dirs f = do
  (pkgDirErrs, packageInfos) <- fmap partitionEithers $ forM dirs $ \dir -> do
    flip fmap (parseCabalPackage dir) $ \case
      Nothing -> Left dir
      Just packageInfo -> Right packageInfo

  when (null packageInfos) $
    failWith $ T.pack $ "No valid pkgs found in " <> intercalate ", " dirs
  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs

  let extensions = packageInfos >>= _cabalPackageInfo_defaultExtensions
      languageFromPkgs = L.nub $ mapMaybe _cabalPackageInfo_defaultLanguage packageInfos
      -- NOTE when no default-language is present cabal sets Haskell98
      language = NE.toList $ fromMaybe (Haskell98 NE.:| []) $ NE.nonEmpty languageFromPkgs
      extensionsLine = if extensions == mempty
        then ""
        else ":set " <> unwords (("-X" <>) . prettyShow <$> extensions)
      ghcOptions = concat $ mapMaybe (\case (GHC, xs) -> Just xs; _ -> Nothing) $
        packageInfos >>= _cabalPackageInfo_compilerOptions
      dotGhci = unlines
        [ ":set -i" <> intercalate ":" (packageInfos >>= rootedSourceDirs)
        , case ghcOptions of
            [] -> ""
            xs -> ":set " <> unwords xs
        , extensionsLine
        , ":set " <> unwords (("-X" <>) . prettyShow <$> language)
        , ":load Backend Frontend"
        , "import Obelisk.Run"
        , "import qualified Frontend"
        , "import qualified Backend"
        ]
  warnDifferentLanguages language
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath

  where
    rootedSourceDirs pkg = NE.toList $
      (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg

warnDifferentLanguages :: MonadObelisk m => [Language] -> m ()
warnDifferentLanguages (_:_:_) = putLog Warning "Different languages detected across packages which may result in errors when loading the repl"
warnDifferentLanguages _ = return ()

-- | Run ghci repl
runGhciRepl
  :: MonadObelisk m
  => FilePath -- ^ Path to .ghci
  -> m ()
runGhciRepl dotGhci = inProjectShell "ghc" $ "ghci " <> makeBaseGhciOptions dotGhci

-- | Run a repl using reflex-ghci
runReflexGhci
  :: MonadObelisk m
  => FilePath
  -> Maybe String
  -> m ()
runReflexGhci dotGhci mcmd = do
  let ghciCmd = P.proc "ghci"
        [ "-Wall"
        , "-ignore-dot-ghci"
        , "-fwarn-redundant-constraints"
        , "-no-user-package-db"
        , "-package-env -"
        , "-ghci-script " <> dotGhci
        ]
  liftIO $ mainWidget $ do
    exitReq <- keyCombo (V.KChar 'c', [V.MCtrl])
    g <- ghciWatch ghciCmd $ T.encodeUtf8 . T.pack <$> mcmd
    ghciPanes g
    exit <- performEvent $ liftIO (P.terminateProcess $ _process_handle $ _ghci_process g) <$ exitReq
    pure $ () <$ exit
  return ()

-- | Run ghcid
runGhcid
  :: MonadObelisk m
  => FilePath -- ^ Path to .ghci
  -> Maybe String -- ^ Optional command to run at every reload
  -> m ()
runGhcid dotGhci mcmd = callCommand $ unwords $ $(staticWhich "ghcid") : opts
  where
    opts =
      [ "-W"
      --TODO: The decision of whether to use -fwarn-redundant-constraints should probably be made by the user
      , "--command='ghci -Wall -ignore-dot-ghci -fwarn-redundant-constraints " <> makeBaseGhciOptions dotGhci <> "' "
      , "--reload=config"
      , "--outputfile=ghcid-output.txt"
      ] <> testCmd
    testCmd = maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'")

makeBaseGhciOptions :: FilePath -> String
makeBaseGhciOptions dotGhci =
  unwords
    [ "-no-user-package-db"
    , "-package-env -"
    , "-ghci-script " <> dotGhci
    ]

getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock
