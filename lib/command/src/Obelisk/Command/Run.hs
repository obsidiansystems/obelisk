{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Run where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.List
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Distribution.PackageDescription.Parse (ParseResult (..), parseGenericPackageDescription)
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic (withUTF8FileContents)
import Network.Socket
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callCommand)
import UnliftIO (MonadUnliftIO, withRunInIO)

import Obelisk.App (MonadObelisk, ObeliskT)
import Obelisk.CLI (Severity (..), failWith, putLog)

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: ObeliskT IO ()
run = do
  freePort <- liftIO getFreePort
  pkgs <- liftIO getLocalPkgs
  (pkgDirErrs, hsSrcDirs) <- fmap partitionEithers $ forM pkgs $ \pkg -> do
    let cabalFp = pkg </> pkg <.> "cabal"
    xs <- parseHsSrcDir cabalFp
    return $ case xs of
      Nothing -> Left pkg
      Just hsSrcDirs -> Right $ toList $ fmap (pkg </>) hsSrcDirs
  when (null hsSrcDirs) $
    failWith $ T.pack $ "No valid pkgs found in " <> intercalate ", " pkgs
  when (not (null pkgDirErrs)) $
    putLog Warning $ T.pack $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs
  let dotGhci = unlines
        [ ":set -i" <> intercalate ":" (mconcat hsSrcDirs)
        , ":add Backend Frontend"
        , ":module + Obelisk.Run Frontend Backend"
        ]
      testCmd = unwords ["Obelisk.Run.run", show freePort , "backend", "frontend"]
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ do
      writeFile dotGhciPath dotGhci
      runDev dotGhciPath $ Just testCmd

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: IO [FilePath]
getLocalPkgs = return ["backend", "common", "frontend"]

parseHsSrcDir
  :: (MonadObelisk m)
  => FilePath -- ^ package cabal file path
  -> m (Maybe (NE.NonEmpty FilePath)) -- ^ List of hs src dirs of the library component
parseHsSrcDir cabalFp = do
  exists <- liftIO $ doesFileExist cabalFp
  if exists
    then do
      withUTF8FileContentsM cabalFp $ \cabal -> do
        case parseGenericPackageDescription cabal of
          ParseOk warnings gpkg -> do
            mapM_ (putLog Warning) $ fmap (T.pack . show) warnings
            return $ do
              (_, lib) <- simplifyCondTree (const $ pure True) <$> condLibrary gpkg
              pure $ fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
          ParseFailed e -> do
            putLog Error $ T.pack $ "Failed to parse cabal file: " <> show e
            return Nothing
    else return Nothing

withUTF8FileContentsM :: MonadUnliftIO m => FilePath -> (String -> m a) -> m a
withUTF8FileContentsM fp f = withRunInIO $ \runInIO ->
  withUTF8FileContents fp $ runInIO . f

-- | Dev
runDev :: FilePath -> Maybe String -> IO ()
runDev dotGhci mcmd = callCommand $ unwords $ "ghcid" : ghcidOpts
  where
    ghcidOpts =
      [ "-W"
      , "--command='ghci -ghci-script " <> dotGhci <> "' "
      , "--reload=config"
      , "--outputfile=ghcid-output.txt"
      ] <> maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'")

getFreePort :: IO PortNumber
getFreePort = withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  E.bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock
