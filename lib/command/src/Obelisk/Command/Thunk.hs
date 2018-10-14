{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command.Thunk
  ( ThunkPtr (..)
  , ThunkRev (..)
  , ThunkSource (..)
  , ThunkData (..)
  , ReadThunkError (..)
  , GitHubSource (..)
  , getThunkGitBranch
  , getLatestRev
  , updateThunkToLatest
  , createThunk
  , createThunkWithLatest
  , nixBuildAttrWithCache
  , nixBuildThunkAttrWithCache
  , unpackThunk
  , packThunk
  , readThunk
  , updateThunk
  , getThunkPtr
  , getThunkPtr'
  , parseGitUri
  , uriThunkPtr
  ) where

import Control.Applicative
import Control.Exception (displayException, throwIO, try)
import Control.Monad
import Control.Monad.Catch (handle)
import Control.Monad.Except
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable
import Data.Git.Ref (Ref)
import qualified Data.Git.Ref as Ref
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Yaml (parseMaybe, (.:))
import qualified Data.Yaml as Yaml
import GitHub
import GitHub.Data.Name
import GitHub.Endpoints.Repos.Contents (archiveForR)
import Network.URI
import Obelisk.Command.Nix
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO.Temp
import System.Posix (getSymbolicLinkStatus, modificationTime)
import System.Process (proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Nix (withNixRemoteCheck)
import Obelisk.Command.Utils

--TODO: Support symlinked thunk data
data ThunkData
   = ThunkData_Packed ThunkPtr
   -- ^ Packed thunk
   | ThunkData_Checkout (Maybe ThunkPtr)
   -- ^ Checked out thunk that was unpacked from this pointer
  deriving (Show, Eq, Ord)

-- | A reference to the exact data that a thunk should translate into
data ThunkPtr = ThunkPtr
  { _thunkPtr_rev :: ThunkRev
  , _thunkPtr_source :: ThunkSource
  }
  deriving (Show, Eq, Ord)

type NixSha256 = Text --TODO: Use a smart constructor and make this actually verify itself

-- | A specific revision of data; it may be available from multiple sources
data ThunkRev = ThunkRev
  { _thunkRev_commit :: Ref
  , _thunkRev_nixSha256 :: NixSha256
  }
  deriving (Show, Eq, Ord)

-- | A location from which a thunk's data can be retrieved
data ThunkSource
   -- | A source specialized for GitHub
   = ThunkSource_GitHub GitHubSource
   -- | A plain repo source
   | ThunkSource_Git GitSource
   deriving (Show, Eq, Ord)

data GitHubSource = GitHubSource
  { _gitHubSource_owner :: Name Owner
  , _gitHubSource_repo :: Name Repo
  , _gitHubSource_branch :: Maybe (Name Branch)
  }
  deriving (Show, Eq, Ord)

data GitSource = GitSource
  { _gitSource_url :: URI
  , _gitSource_branch :: Maybe (Name Branch)
  , _gitSource_fetchSubmodules :: Bool
  }
  deriving (Show, Eq, Ord)

getThunkGitBranch :: ThunkPtr -> Maybe Text
getThunkGitBranch (ThunkPtr _ src) = fmap untagName $ case src of
  ThunkSource_GitHub s -> _gitHubSource_branch s
  ThunkSource_Git s -> _gitSource_branch s

commitNameToRef :: Name Commit -> Ref
commitNameToRef (N c) = Ref.fromHex $ encodeUtf8 c

-- TODO: Use spinner here.
getNixSha256ForUriUnpacked
  :: MonadObelisk m
  => URI
  -> m NixSha256
getNixSha256ForUriUnpacked uri =
  withExitFailMessage ("nix-prefetch-url: Failed to determine sha256 hash of URL " <> T.pack (show uri)) $ do
    withNixRemoteCheck $ readProcessAndLogStderr Debug $
      proc "nix-prefetch-url" ["--unpack" , "--type" , "sha256" , show uri]

nixPrefetchGit :: MonadObelisk m => URI -> Text -> Bool -> m NixSha256
nixPrefetchGit uri rev fetchSubmodules =
  withExitFailMessage ("nix-prefetch-git: Failed to determine sha256 hash of Git repo " <> T.pack (show uri) <> " at " <> rev) $ do
    out <- withNixRemoteCheck $ readProcessAndLogStderr Debug $
      proc "nix-prefetch-git" $ filter (/="")
        [ "--url", show uri
        , "--rev", T.unpack rev
        , if fetchSubmodules then "--fetch-submodules" else ""
        , "--quiet"
        ]

    case parseMaybe (Aeson..: "sha256") =<< Aeson.decodeStrict (encodeUtf8 out) of
      Nothing -> failWith $ "nix-prefetch-git: unrecognized output " <> out
      Just x -> pure x

-- | Get the latest revision available from the given source
getLatestRev :: MonadObelisk m => ThunkSource -> m ThunkRev
getLatestRev = \case
  ThunkSource_GitHub s -> do
    auth <- liftIO $ getHubAuth "github.com"
    commitsResult <- liftIO $ executeRequestMaybe auth $ commitsWithOptionsForR
      (_gitHubSource_owner s)
      (_gitHubSource_repo s)
      (FetchAtLeast 1)
      $ case _gitHubSource_branch s of
          Nothing -> []
          Just b -> [CommitQuerySha $ untagName b]
    commitInfos <- either (liftIO . throwIO) return commitsResult
    commitInfo : _ <- return $ toList commitInfos
    let commit = commitSha commitInfo
    putLog Debug $ "Latest commit is " <> untagName commit
    githubThunkPtr s $ untagName commit
  ThunkSource_Git s -> do
    (_, commit) <- gitGetCommitBranch (_gitSource_url s) (untagName <$> _gitSource_branch s)
    gitThunkPtr s commit

--TODO: Pretty print these
data ReadThunkError
   = ReadThunkError_UnrecognizedFiles
   | ReadThunkError_AmbiguousFiles
   | ReadThunkError_UnrecognizedLoader Text
   | ReadThunkError_UnparseablePtr LBS.ByteString
   deriving (Show)

-- | Read a thunk and validate that it is only a thunk, either packed or unpacked.
-- If the thunk is packed and additional data is present, fail.
readThunk :: MonadObelisk m => FilePath -> m (Either ReadThunkError ThunkData)
readThunk thunkDir = do
  files <- liftIO $ listDirectory thunkDir
  case ".git" `elem` files of
    True -> fmap ThunkData_Checkout <$> do
      let origThunkPath = thunkDir </> ".git" </> "obelisk" </> "orig-thunk"
      liftIO $ doesDirectoryExist origThunkPath >>= \case
        True -> fmap Just <$> readPackedThunk origThunkPath
        False -> return $ return Nothing
    False -> liftIO $ fmap ThunkData_Packed <$> readPackedThunk thunkDir

data ThunkType = ThunkType
  { _thunkType_loader :: FilePath
  , _thunkType_json :: FilePath
  , _thunkType_optional :: Set FilePath
  , _thunkType_loaderVersions :: NonEmpty Text
  , _thunkType_parser :: Aeson.Object -> Aeson.Parser ThunkPtr
  }

gitHubThunkType :: ThunkType
gitHubThunkType = ThunkType
  { _thunkType_loader = "default.nix"
  , _thunkType_json = "github.json"
  , _thunkType_optional = Set.fromList [".attr-cache"]
  , _thunkType_loaderVersions = gitHubStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ \v ->
      ThunkSource_GitHub <$> parseGitHubSource v <|> ThunkSource_Git <$> parseGitSource v
  }

gitThunkType :: ThunkType
gitThunkType = ThunkType
  { _thunkType_loader = "default.nix"
  , _thunkType_json = "git.json"
  , _thunkType_optional = Set.fromList [".attr-cache"]
  , _thunkType_loaderVersions = plainGitStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ fmap ThunkSource_Git . parseGitSource
  }

thunkTypes :: [ThunkType]
thunkTypes = [gitThunkType, gitHubThunkType]

findThunkType :: [ThunkType] -> FilePath -> IO (Either ReadThunkError ThunkType)
findThunkType types thunkDir = do
  matches <- fmap catMaybes $ forM types $ \thunkType -> do
    let
      expectedContents = Set.fromList $ (thunkDir </>) <$>
        [ _thunkType_loader thunkType
        , _thunkType_json thunkType
        ]
      optionalContents = Set.map (thunkDir </>) (_thunkType_optional thunkType)

    -- Ensure that there aren't any other files in the thunk
    -- NB: System.Directory.listDirectory returns the contents without the directory path
    files <- liftIO $ Set.fromList . fmap (thunkDir </>) <$> listDirectory thunkDir
    let unexpectedContents = files `Set.difference` (expectedContents `Set.union` optionalContents)
        missingContents = expectedContents `Set.difference` files
    pure $ if Set.null unexpectedContents && Set.null missingContents
      then Just thunkType
      else Nothing

  pure $ case matches of
    [singleMatch] -> Right singleMatch
    [] -> Left ReadThunkError_UnrecognizedFiles
    _ -> Left ReadThunkError_AmbiguousFiles

-- | Read a thunk and validate that it is exactly a packed thunk.
-- If additional data is present, fail.
readPackedThunk :: FilePath -> IO (Either ReadThunkError ThunkPtr)
readPackedThunk thunkDir = runExceptT $ do
  thunkType <- ExceptT $ findThunkType thunkTypes thunkDir
  -- Ensure that we recognize the thunk loader
  loader <- liftIO $ T.readFile $ thunkDir </> _thunkType_loader thunkType
  unless (loader `elem` _thunkType_loaderVersions thunkType) $ do
    throwError $ ReadThunkError_UnrecognizedLoader loader

  txt <- liftIO $ LBS.readFile $ thunkDir </> _thunkType_json thunkType
  case parseMaybe (_thunkType_parser thunkType) =<< Aeson.decode txt of
    Nothing -> throwError $ ReadThunkError_UnparseablePtr txt
    Just ptr -> return ptr

parseThunkPtr :: (Aeson.Object -> Aeson.Parser ThunkSource) -> Aeson.Object -> Aeson.Parser ThunkPtr
parseThunkPtr parseSrc v = do
  rev <- v Aeson..: "rev"
  sha256 <- v Aeson..: "sha256"
  src <- parseSrc v
  pure $ ThunkPtr
    { _thunkPtr_rev = ThunkRev
      { _thunkRev_commit = Ref.fromHexString rev
      , _thunkRev_nixSha256 = sha256
      }
    , _thunkPtr_source = src
    }

parseGitHubSource :: Aeson.Object -> Aeson.Parser GitHubSource
parseGitHubSource v = do
  owner <- v Aeson..: "owner"
  repo <- v Aeson..: "repo"
  branch <- v Aeson..:! "branch"
  pure $ GitHubSource
    { _gitHubSource_owner = owner
    , _gitHubSource_repo = repo
    , _gitHubSource_branch = branch
    }

parseGitSource :: Aeson.Object -> Aeson.Parser GitSource
parseGitSource v = do
  Just url <- parseGitUri <$> v Aeson..: "url"
  branch <- v Aeson..:! "branch"
  fetchSubmodules <- v Aeson..:! "fetchSubmodules"
  pure $ GitSource
    { _gitSource_url = url
    , _gitSource_branch = branch
    , _gitSource_fetchSubmodules = fromMaybe False fetchSubmodules
    }

overwriteThunk :: MonadObelisk m => FilePath -> ThunkPtr -> m ()
overwriteThunk target thunk = do
  -- Ensure that this directory is a valid thunk (i.e. so we aren't losing any data)
  Right _ <- readThunk target

  --TODO: Is there a safer way to do this overwriting?
  liftIO $ removeDirectoryRecursive target
  liftIO $ createThunk target thunk

thunkPtrLoader :: ThunkPtr -> Text
thunkPtrLoader thunk = case _thunkPtr_source thunk of
  ThunkSource_GitHub _ -> NonEmpty.head gitHubStandaloneLoaders
  ThunkSource_Git _ -> NonEmpty.head plainGitStandaloneLoaders

-- It's important that formatting be very consistent here, because
-- otherwise when people update thunks, their patches will be messy
encodeThunkPtrData :: ThunkPtr -> LBS.ByteString
encodeThunkPtrData (ThunkPtr rev src) = case src of
  ThunkSource_GitHub s -> encodePretty' githubCfg $ Aeson.object $ catMaybes
    [ Just $ "owner" .= _gitHubSource_owner s
    , Just $ "repo" .= _gitHubSource_repo s
    , ("branch" .=) <$> _gitHubSource_branch s
    , Just $ "rev" .= Ref.toHexString (_thunkRev_commit rev)
    , Just $ "sha256" .= _thunkRev_nixSha256 rev
    ]
  ThunkSource_Git s -> encodePretty' plainGitCfg $ Aeson.object $ catMaybes
    --TODO: Is this safe? read/show is jank for `URI`
    [ Just $ "url" .= show (_gitSource_url s)
    , Just $ "rev" .= Ref.toHexString (_thunkRev_commit rev)
    , ("branch" .=) <$> _gitSource_branch s
    , Just $ "sha256" .= _thunkRev_nixSha256 rev
    , Just $ "fetchSubmodules" .= _gitSource_fetchSubmodules s
    ]
 where
  githubCfg = defConfig
    { confIndent = Spaces 2
    , confCompare = keyOrder
        [ "owner"
        , "repo"
        , "branch"
        , "private"
        , "rev"
        , "sha256"
        ] <> compare
    , confTrailingNewline = True
    }
  plainGitCfg = defConfig
    { confIndent = Spaces 2
    , confCompare = keyOrder
        [ "url"
        , "rev"
        , "sha256"
        , "fetchSubmodules"
        ] <> compare
    , confTrailingNewline = True
    }

createThunk :: MonadIO m => FilePath -> ThunkPtr -> m ()
createThunk target thunk = liftIO $ do
  createDirectoryIfMissing True (target </> ".attr-cache")
  T.writeFile (target </> "default.nix") (thunkPtrLoader thunk)
  let
    jsonFileName = case _thunkPtr_source thunk of
      ThunkSource_GitHub _ -> "github"
      ThunkSource_Git _ -> "git"
  LBS.writeFile (target </> jsonFileName <.> "json") (encodeThunkPtrData thunk)

createThunkWithLatest :: MonadObelisk m => FilePath -> ThunkSource -> m ()
createThunkWithLatest target s = do
  rev <- getLatestRev s
  createThunk target $ ThunkPtr
    { _thunkPtr_source = s
    , _thunkPtr_rev = rev
    }

updateThunkToLatest :: MonadObelisk m => FilePath -> m ()
updateThunkToLatest target = withSpinner' ("Updating thunk " <> T.pack target <> " to latest") (pure $ const $ "Thunk " <> T.pack target <> " updated to latest") $ do
  (overwrite, ptr) <- readThunk target >>= \case
    Left err -> failWith $ T.pack $ "thunk update: " <> show err
    Right c -> case c of
      ThunkData_Packed t -> return (target, t)
      ThunkData_Checkout _ -> failWith "cannot update an unpacked thunk"
  let src = _thunkPtr_source ptr
  rev <- getLatestRev src
  overwriteThunk overwrite $ ThunkPtr
    { _thunkPtr_source = src
    , _thunkPtr_rev = rev
    }

-- | All recognized github standalone loaders, ordered from newest to oldest.
-- This tool will only ever produce the newest one when it writes a thunk.
gitHubStandaloneLoaders :: NonEmpty Text
gitHubStandaloneLoaders =
  gitHubStandaloneLoaderV2 :|
  [ gitHubStandaloneLoaderV3
  , gitHubStandaloneLoaderV1
  ]

gitHubStandaloneLoaderV1 :: Text
gitHubStandaloneLoaderV1 = T.unlines
  [ "import ((import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./github.json)))"
  ]

gitHubStandaloneLoaderV2 :: Text
gitHubStandaloneLoaderV2 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE" --TODO: Add something about how to get more info on Obelisk, etc.
  , "import ((import <nixpkgs> {}).fetchFromGitHub ("
  , "  let json = builtins.fromJSON (builtins.readFile ./github.json);"
  , "  in { inherit (json) owner repo rev sha256;"
  , "       private = json.private or false;"
  , "     }"
  , "))"
  ]

gitHubStandaloneLoaderV3 :: Text
gitHubStandaloneLoaderV3 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let"
  , "  fetch = { private ? false, ... }@args: if private && builtins.hasAttr \"fetchGit\" builtins"
  , "    then fetchFromGitHubPrivate args"
  , "    else (import <nixpkgs> {}).fetchFromGitHub (builtins.removeAttrs args [\"branch\"]);"
  , "  fetchFromGitHubPrivate ="
  , "    { owner, repo, rev, branch ? null, name ? null, sha256 ? null, private ? false"
  , "    , fetchSubmodules ? false, githubBase ? \"github.com\", ..."
  , "    }: assert !fetchSubmodules;"
  , "      builtins.fetchGit ({"
  , "        url = \"ssh://git@${githubBase}/${owner}/${repo}.git\";"
  , "        inherit rev;"
  , "      }"
  , "      // (if branch == null then {} else { ref = branch; })"
  , "      // (if name == null then {} else { inherit name; }));"
  , "in import (fetch (builtins.fromJSON (builtins.readFile ./github.json)))"
  ]

plainGitStandaloneLoaders :: NonEmpty Text
plainGitStandaloneLoaders =
  plainGitStandaloneLoaderV1 :|
  [ plainGitStandaloneLoaderV2
  ]

plainGitStandaloneLoaderV1 :: Text
plainGitStandaloneLoaderV1 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let fetchGit = {url, rev, ref ? null, branch ? null, sha256 ? null, fetchSubmodules ? null}:"
  , "  assert !fetchSubmodules; (import <nixpkgs> {}).fetchgit { inherit url rev sha256; };"
  , "in import (fetchGit (builtins.fromJSON (builtins.readFile ./git.json)))"
  ]

plainGitStandaloneLoaderV2 :: Text
plainGitStandaloneLoaderV2 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let fetchGit = {url, rev, ref ? null, branch ? null, sha256 ? null, fetchSubmodules ? null}:"
  , "  if builtins.hasAttr \"fetchGit\" builtins"
  , "    then builtins.fetchGit ({ inherit url rev; } // (if branch == null then {} else { ref = branch; }))"
  , "    else abort \"Plain Git repositories are only supported on nix 2.0 or higher.\";"
  , "in import (fetchGit (builtins.fromJSON (builtins.readFile ./git.json)))"
  ]

thunkFileNames :: [FilePath]
thunkFileNames = ["default.nix", "github.json", "git.json"]

-- | Look for authentication info from the 'hub' command
getHubAuth
  :: Text -- ^ Domain name
  -> IO (Maybe Auth)
getHubAuth domain = do
  hubConfig <- getXdgDirectory XdgConfig "hub"
  Yaml.decodeFileEither hubConfig >>= \case
    Left _ -> return Nothing
    Right v -> return $ flip parseMaybe v $ \v' -> do
      Yaml.Array domainConfigs <- v' .: domain --TODO: Determine what multiple domainConfigs means
      [Yaml.Object domainConfig] <- return $ toList domainConfigs
      token <- domainConfig .: "oauth_token"
      return $ OAuth $ encodeUtf8 token

--TODO: when checking something out, make a shallow clone

-- | Checks a cache directory to see if there is a fresh symlink
-- to the result of building an attribute of a thunk.
-- If no cache hit is found, nix-build is called to build the attribute
-- and the result is symlinked into the cache.
nixBuildThunkAttrWithCache
  :: MonadObelisk m
  => FilePath
  -- ^ Path to directory containing Thunk
  -> String
  -- ^ Attribute to build
  -> m FilePath
  -- ^ Symlink to cached or built nix output
-- WARNING: If the thunk uses an impure reference such as '<nixpkgs>'
-- the caching mechanism will fail as it merely measures the modification
-- time of the cache link and the expression to build.
nixBuildThunkAttrWithCache thunkDir attr = do
  --NB: Expects thunkDir to be normalised with no trailing path separator.
  --This should be guaranteed by the command argument parser.
  let cacheErrHandler e
        | isDoesNotExistError e = return Nothing -- expected from a cache miss
        | otherwise = putLog Error (T.pack $ displayException e) >> return Nothing
      cacheDir = thunkDir </> ".attr-cache"
      cachePath = cacheDir </> attr <.> "out"
  latestChange <- liftIO $ do
    createDirectoryIfMissing False cacheDir
    let getModificationTimeMaybe = fmap rightToMaybe . try @IOError . getModificationTime
    maximum . catMaybes <$> mapM (getModificationTimeMaybe . (thunkDir </>)) thunkFileNames
  cacheHit <- handle cacheErrHandler $ do
    cacheTime <- liftIO $ posixSecondsToUTCTime . realToFrac . modificationTime <$> getSymbolicLinkStatus cachePath
    return $ if latestChange <= cacheTime
      then Just cachePath
      else Nothing
  case cacheHit of
    Just c -> return c
    Nothing -> do
      putLog Warning $ T.pack $ mconcat [thunkDir, ": ", attr, " not cached, building ..."]
      _ <- nixBuild $ def
        { _nixBuildConfig_target = Target
          { _target_path = thunkDir
          , _target_attr = Just attr
          }
        , _nixBuildConfig_outLink = OutLink_IndirectRoot cachePath
        }
      return cachePath

-- | Build a nix attribute, and cache the result if possible
nixBuildAttrWithCache
  :: MonadObelisk m
  => FilePath
  -- ^ Path to directory containing Thunk
  -> String
  -- ^ Attribute to build
  -> m FilePath
  -- ^ Symlink to cached or built nix output
nixBuildAttrWithCache exprPath attr = do
  readThunk exprPath >>= \case
    -- Only packed thunks are cached. in particular, checkouts are not
    Right (ThunkData_Packed _) -> nixBuildThunkAttrWithCache exprPath attr
    _ -> nixBuild $ def
      { _nixBuildConfig_target = Target
        { _target_path = exprPath
        , _target_attr = Just attr
        }
      , _nixBuildConfig_outLink = OutLink_None
      }

-- | Safely update thunk using a custom action
--
-- A temporary working space is used to do any update. When the custom
-- action successfully completes, the resulting (packed) thunk is copied
-- back to the original location.
updateThunk :: MonadObelisk m => FilePath -> (FilePath -> m a) -> m a
updateThunk p f = withSystemTempDirectory "obelisk-thunkptr-" $ \tmpDir -> do
  p' <- copyThunkToTmp tmpDir p
  unpackThunk' True p'
  result <- f p'
  updateThunkFromTmp p' "origin"  -- TODO: stop hardcoding remote
  return result
  where
    copyThunkToTmp tmpDir thunkDir = readThunk thunkDir >>= \case
      Left err -> failWith $ "withThunkUnpacked: " <> T.pack (show err)
      Right (ThunkData_Packed _) -> do
        let tmpThunk = tmpDir </> "thunk"
        callProcessAndLogOutput (Notice, Error) $
          proc "cp" ["-r", "-T", thunkDir, tmpThunk]
        return tmpThunk
      Right _ -> failWith $ "Thunk is not packed"
    updateThunkFromTmp p' remote = do
      packThunk' True p' remote
      callProcessAndLogOutput (Notice, Error) $
        proc "cp" ["-r", "-T", p', p]

finalMsg :: Bool -> (a -> Text) -> Maybe (a -> Text)
finalMsg noTrail s = if noTrail then Nothing else Just s

unpackThunk :: MonadObelisk m => FilePath -> m ()
unpackThunk = unpackThunk' False

unpackThunk' :: MonadObelisk m => Bool -> FilePath -> m ()
unpackThunk' noTrail thunkDir = readThunk thunkDir >>= \case
  Left err -> failWith $ "thunk unpack: " <> T.pack (show err)
  --TODO: Overwrite option that rechecks out thunk; force option to do so even if working directory is dirty
  Right (ThunkData_Checkout _) -> failWith "thunk unpack: thunk is already unpacked"
  Right (ThunkData_Packed tptr) -> do
    let (thunkParent, thunkName) = splitFileName thunkDir
    withTempDirectory thunkParent thunkName $ \tmpRepo -> do
      let obGitDir = tmpRepo </> ".git" </> "obelisk"
      case _thunkPtr_source tptr of
        ThunkSource_GitHub s -> do
          mauth <- liftIO $ getHubAuth "github.com"
          repoResult <- liftIO $ executeRequestMaybe mauth $ repositoryR (_gitHubSource_owner s) (_gitHubSource_repo s)
          githubURI <- either (liftIO . throwIO) (return . repoSshUrl) repoResult >>= \case
            Nothing -> failWith "Cannot determine clone URI for thunk source"
            Just c -> return $ T.unpack $ getUrl c

          withSpinner' ("Fetching thunk " <> T.pack thunkDir)
                       ( finalMsg noTrail $ const $ "Fetched thunk " <> T.pack thunkDir) $ do
            callProcessAndLogOutput (Debug, Debug) $
              proc "hub" ["clone", "-n", githubURI, tmpRepo]
            --If this directory already exists then something is weird and we should fail
            liftIO $ createDirectory obGitDir
            callProcessAndLogOutput (Debug, Error) $
              proc "cp" ["-r", "-T", thunkDir </> ".", obGitDir </> "orig-thunk"]

            -- Checkout
            putLog Debug $ "Checking out " <> T.pack (show $ maybe "" untagName $ _gitHubSource_branch s)
            callProcessAndLogOutput (Debug, Debug) $
              proc "hub" $ concat
                [ ["-C", tmpRepo]
                , pure "checkout"
                , maybe [] (\n -> ["-B", T.unpack (untagName n)]) (_gitHubSource_branch s)
                , pure $ Ref.toHexString $ _thunkRev_commit $ _thunkPtr_rev tptr
                ]
            -- Set upstream branch
            forM_ (_gitHubSource_branch s) $ \branch ->
              callProcessAndLogOutput (Debug, Error) $
                proc "hub" ["-C", tmpRepo, "branch", "-u", "origin/" <> T.unpack (untagName branch)]
            callProcessAndLogOutput (Notice, Error) $ proc "rm" ["-r", thunkDir]
            callProcessAndLogOutput (Notice, Error) $ proc "mv" ["-T", tmpRepo, thunkDir]

        ThunkSource_Git s -> do
          withSpinner' ("Fetching thunk " <> T.pack thunkName)
                       (finalMsg noTrail $ const $ "Fetched thunk " <> T.pack thunkName) $ do
            let git = callProcessAndLogOutput (Notice, Notice) . gitProc tmpRepo
            git ["clone", if _gitSource_fetchSubmodules s then "--recursive" else "", show (_gitSource_url s)]
            git ["reset", "--hard", Ref.toHexString $ _thunkRev_commit $ _thunkPtr_rev tptr]
            when (_gitSource_fetchSubmodules s) $
              git ["submodule", "update", "--recursive", "--init"]
            case _gitSource_branch s of
              Just b -> git ["branch", "-u", "origin/" <> T.unpack (untagName $ b)]
              Nothing -> pure ()

            liftIO $ createDirectory obGitDir
            callProcessAndLogOutput (Notice, Error) $
              proc "cp" ["-r", "-T", thunkDir </> ".", obGitDir </> "orig-thunk"]
            callProcessAndLogOutput (Notice, Error) $
              proc "rm" ["-r", thunkDir]
            callProcessAndLogOutput (Notice, Error) $
              proc "mv" ["-T", tmpRepo, thunkDir]

--TODO: add force mode to pack even if changes are present
--TODO: add a rollback mode to pack to the original thunk
packThunk :: MonadObelisk m => FilePath -> Text -> m ()
packThunk = packThunk' False

packThunk' :: MonadObelisk m => Bool -> FilePath -> Text -> m ()
packThunk' noTrail thunkDir upstream = readThunk thunkDir >>= \case
  Left err -> failWith $ T.pack $ "thunk pack: " <> show err
  Right (ThunkData_Packed _) -> failWith "pack: thunk is already packed"
  Right (ThunkData_Checkout _) -> do
    withSpinner' ("Packing thunk " <> T.pack thunkDir)
                 (finalMsg noTrail $ const $ "Packed thunk " <> T.pack thunkDir) $ do
      thunkPtr <- getThunkPtr thunkDir upstream
      callProcessAndLogOutput (Debug, Error) $ proc "rm" ["-rf", thunkDir]
      liftIO $ createThunk thunkDir thunkPtr

getThunkPtr :: MonadObelisk m => FilePath -> Text -> m ThunkPtr
getThunkPtr = getThunkPtr' True

getThunkPtr' :: MonadObelisk m => Bool -> FilePath -> Text -> m ThunkPtr
getThunkPtr' checkClean thunkDir upstream = do
    when checkClean $ ensureCleanGitRepo thunkDir True $
      "thunk pack: thunk checkout contains unsaved modifications"

    -- Check whether there are any stashes
    stashOutput <- readGitProcess thunkDir ["stash", "list"]
    when checkClean $ case T.null stashOutput of
      False -> do
        failWith $ T.unlines $
          [ "thunk pack: thunk checkout has stashes"
          , "git stash list:"
          ] ++ T.lines stashOutput
      True -> return ()

    --Check whether all local heads are pushed
    repoHeads <- T.lines <$> readGitProcess thunkDir ["for-each-ref", "--format=%(refname:short)", "refs/heads/"]
    remotes <- T.lines <$> readGitProcess thunkDir ["remote"]
    --Check that the upstream specified actually exists
    case L.find (== upstream) remotes of
      Nothing -> failWith $ "thunk pack: upstream " <> upstream <> " does not exist. Available upstreams are " <> T.intercalate ", " remotes
      Just _ -> return ()
    -- iterate over cartesian product
    when checkClean $ forM_ repoHeads $ \hd -> do
      [localRev] <- T.lines <$> readGitProcess thunkDir ["rev-parse", T.unpack hd]
      forM_ remotes $ \rm -> do
        --TODO: The error you get if a branch isn't pushed anywhere could be made more user friendly
        [remoteMergeRev] <- fmap T.lines $ readGitProcess thunkDir
          ["merge-base", T.unpack localRev, "remotes/" <> T.unpack rm <> "/" <> T.unpack hd]
        case remoteMergeRev == localRev of
          False -> failWith $ mconcat [ "thunk unpack: branch ", hd, " has not been pushed to ", rm ]
          True -> return ()

    --We assume it's safe to pack the thunk at this point
    [remoteUri'] <- fmap T.lines $ readGitProcess thunkDir ["config", "--get", "remote." <> T.unpack upstream <> ".url"]

    let uriParseFailure = "Could not identify git remote: " <> remoteUri'
        mRemoteUri = parseGitUri remoteUri'
    case mRemoteUri of
      Nothing -> failWith uriParseFailure
      Just remoteUri -> do
        refs <- fmap (map (refsToTuples . T.words) . T.lines) $ readGitProcess thunkDir ["show-ref", "--head"]
        -- safe because exactly one hash is associated with HEAD
        let Just currentHead = fst <$> L.find ((== "HEAD") . snd) refs
            remoteHeads = Map.fromList $ catMaybes $ fmap
              (\(x,y) -> (,) x <$> T.stripPrefix ("refs/remotes/" <> upstream <> "/") y)
              refs
            localHeads = Map.fromList $ catMaybes $ fmap
              (\(x,y) -> (,) x <$> T.stripPrefix "refs/heads/" y)
              refs
            mCurrentUpstreamBranch = Map.lookup currentHead remoteHeads
                                 <|> Map.lookup currentHead localHeads
        let src = uriToThunkSource remoteUri mCurrentUpstreamBranch
        rev <- case src of
          ThunkSource_GitHub s -> githubThunkPtr s currentHead
          ThunkSource_Git s -> gitThunkPtr s currentHead
        pure $ ThunkPtr
          { _thunkPtr_rev = rev
          , _thunkPtr_source = src
          }
 where
  refsToTuples [x, y] = (x, y)
  refsToTuples x = error $ "thunk pack: cannot parse ref " <> show x

-- | N.B. Cannot infer all fields.
uriToThunkSource :: URI -> Maybe Text -> ThunkSource
uriToThunkSource u
  | Just uriAuth <- uriAuthority u
  , case uriScheme u of
       "ssh:" -> uriAuth == URIAuth "git@" "github.com" ""
       s -> s `L.elem` [ "git:", "https:", "http:" ] -- "http:" just redirects to "https:"
         && uriRegName uriAuth == "github.com"
  , ["/", owner, repo] <- splitDirectories (uriPath u)
  = \mbranch -> ThunkSource_GitHub $ GitHubSource
    { _gitHubSource_owner = N $ T.pack owner
    , _gitHubSource_repo = N $ T.pack repo
    , _gitHubSource_branch = N <$> mbranch
    }

  | otherwise = \mbranch -> ThunkSource_Git $ GitSource
    { _gitSource_url = u
    , _gitSource_branch = N <$> mbranch
    , _gitSource_fetchSubmodules = False -- TODO: How do we determine if this should be true?
    }

-- Funny signature indicates no effects depend on the optional branch name.
githubThunkPtr
  :: MonadObelisk m
  => GitHubSource
  -> Text
  -> m ThunkRev
githubThunkPtr s commit = do
  mauth <- liftIO $ getHubAuth "github.com"
  archiveResult <- liftIO $ executeRequestMaybe mauth $
    archiveForR (_gitHubSource_owner s) (_gitHubSource_repo s) ArchiveFormatTarball (Just commit)
  archiveUri <- either (liftIO . throwIO) return archiveResult
  hash <- getNixSha256ForUriUnpacked archiveUri
  putLog Debug $ "Nix sha256 is " <> hash
  return $ ThunkRev
    { _thunkRev_commit = commitNameToRef $ N commit
    , _thunkRev_nixSha256 = hash
    }

gitThunkPtr
  :: MonadObelisk m
  => GitSource
  -> Text
  -> m ThunkRev
gitThunkPtr s commit = do
  let u = _gitSource_url s
      protocols = ["https:", "ssh:", "git:"]
  unless (T.toLower (T.pack $ uriScheme u) `elem` protocols) $
    failWith $ "obelisk currently only supports "
      <> T.intercalate ", " protocols <> " protocols for non-GitHub remotes"
  hash <- nixPrefetchGit u commit $ _gitSource_fetchSubmodules s
  putLog Debug $ "Nix sha256 is " <> hash
  pure $ ThunkRev
    { _thunkRev_commit = commitNameToRef (N commit)
    , _thunkRev_nixSha256 = hash
    }

gitGetCommitBranch
  :: MonadObelisk m => URI -> Maybe Text -> m (Text, Text)
gitGetCommitBranch uri mbranch = withExitFailMessage ("Failure for git remote " <> uriMsg) $ do
  bothMaps <- rethrowE =<< liftIO (gitLsRemote (show uri) (GitRef_Branch <$> mbranch))
  branch <- case mbranch of
    Nothing -> withExitFailMessage "Failed to find default branch" $ do
      b <- rethrowE $ gitLookupDefaultBranch bothMaps
      putLog Debug $ "Default branch for remote repo " <> uriMsg <> " is " <> b
      pure b
    Just b -> pure b
  commit <- rethrowE $ gitLookupCommitForRef bothMaps (GitRef_Branch branch)
  putLog Debug $ "Latest commit in branch " <> branch
    <> " from remote repo " <> uriMsg
    <> " is " <> commit
  pure (branch, commit)
  where
    rethrowE = either failWith pure
    uriMsg = T.pack $ show uri

uriThunkPtr :: MonadObelisk m => URI -> Maybe Text -> m ThunkPtr
uriThunkPtr uri mbranch = do
  (_, commit) <- gitGetCommitBranch uri mbranch
  let src = uriToThunkSource uri mbranch
  rev <- case src of
        ThunkSource_GitHub s -> githubThunkPtr s commit
        ThunkSource_Git s -> gitThunkPtr s commit
  pure $ ThunkPtr
    { _thunkPtr_rev = rev
    , _thunkPtr_source = src
    }

parseGitUri :: Text -> Maybe URI
parseGitUri x = parseURIReference (T.unpack x) <|> parseSshShorthand x

parseSshShorthand :: Text -> Maybe URI
parseSshShorthand uri = do
  -- This is what git does to check that the remote
  -- is not a local file path when parsing shorthand.
  -- Last referenced from here:
  -- https://github.com/git/git/blob/95ec6b1b3393eb6e26da40c565520a8db9796e9f/connect.c#L712
  let
    (authAndHostname, colonAndPath) = T.break (== ':') uri
    properUri = "ssh://" <> authAndHostname <> "/" <> T.drop 1 colonAndPath
  -- Shorthand is valid iff a colon is present and it occurs before the first slash
  -- This check is used to disambiguate a filepath containing a colon from shorthand
  guard $ isNothing (T.findIndex (=='/') authAndHostname)
        && not (T.null colonAndPath)
  parseURI $ T.unpack properUri
