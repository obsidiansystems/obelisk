{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Run where

import qualified Control.Exception as E
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.List.NonEmpty as NE
import Distribution.Utils.Generic (withUTF8FileContents)
import Distribution.PackageDescription.Parse (parseGenericPackageDescription, ParseResult(..))
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Library
import Distribution.Types.GenericPackageDescription
import Network.Socket
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callCommand)

run :: IO ()
run = do
  freePort <- getFreePort
  pkgs <- getLocalPkgs
  (pkgDirErrs, hsSrcDirs) <- fmap partitionEithers $ forM pkgs $ \pkg -> do
    let cabalFp = pkg </> pkg <.> "cabal"
    xs <- parseHsSrcDir cabalFp
    return $ case xs of
      Nothing -> Left pkg
      Just hsSrcDirs -> Right $ toList $ fmap (pkg </>) hsSrcDirs
  when (null hsSrcDirs) $
    fail $ "No valid pkgs found in " <> intercalate ", " pkgs
  when (not (null pkgDirErrs)) $
    putStrLn $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs
  let dotGhci = unlines
        [ ":set args --quiet --port " <> show freePort
        , ":set -i" <> intercalate ":" (mconcat hsSrcDirs)
        , ":add Backend Frontend"
        , ":module + Control.Concurrent Obelisk.Widget.Run Frontend Backend"
        ]
      testCmd = unlines
        [ "backendId <- forkIO backend"
        , "let conf = defRunConfig { _runConfig_redirectPort = " <> show freePort <> "}"
        , "runWidget conf frontend"
        , "killThread backendId"
        ]
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    writeFile dotGhciPath dotGhci
    runDev dotGhciPath $ Just testCmd

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: IO [FilePath]
getLocalPkgs = return ["backend", "common", "frontend"]

parseHsSrcDir :: FilePath -- ^ package cabal file path
              -> IO (Maybe (NE.NonEmpty FilePath)) -- ^ List of hs src dirs of the library component
parseHsSrcDir cabalFp = do
  exists <- doesFileExist cabalFp
  if exists
    then do
      withUTF8FileContents cabalFp $ \cabal -> do
      case parseGenericPackageDescription cabal of
        ParseOk warnings gpkg -> do
          mapM_ print warnings
          return $ do
            (_, lib) <- simplifyCondTree (const $ pure True) <$> condLibrary gpkg
            pure $ fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
        ParseFailed _ -> return Nothing
    else return Nothing

-- | Dev
runDev :: FilePath -> Maybe String -> IO ()
runDev dotGhci mcmd = callCommand $ unwords $ "ghcid" : ghcidOpts
  where
    ghcidOpts =
      [ "-W"
      , "--command='ghci -ghci-script " <> dotGhci <> "' "
      ] <> maybeToList (flip fmap mcmd $ \cmd -> " --test=$'" <> cmd <> "'")

getFreePort :: IO PortNumber
getFreePort = withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  E.bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock
