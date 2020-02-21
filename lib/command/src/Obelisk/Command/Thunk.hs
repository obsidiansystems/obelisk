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
  ( GitHubSource (..)
  , GitUri (..)
  , ReadThunkError (..)
  , ThunkConfig (..)
  , ThunkData (..)
  , ThunkPackConfig (..)
  , ThunkPtr (..)
  , ThunkRev (..)
  , ThunkSource (..)
  , ThunkUpdateConfig (..)
  , attrCacheFileName
  , createThunk
  , createThunkWithLatest
  , getLatestRev
  , getThunkGitBranch
  , getThunkPtr
  , gitUriToText
  , nixBuildAttrWithCache
  , nixBuildThunkAttrWithCache
  , packThunk
  , parseGitUri
  , readThunk
  , unpackThunk
  , updateThunk
  , updateThunkToLatest
  , uriThunkPtr
  ) where

import Control.Applicative
import Control.Exception (displayException, try)
import qualified Control.Lens as Lens
import Control.Lens.Indexed hiding ((<.>))
import Control.Monad
import Control.Monad.Catch (handle)
import Control.Monad.Except
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Default
import Data.Either.Combinators (fromRight', rightToMaybe)
import Data.Git.Ref (Ref)
import qualified Data.Git.Ref as Ref
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Yaml (parseMaybe)
import GitHub
import GitHub.Data.Name
import Obelisk.Command.Nix
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Error
import System.IO.Temp
import System.Posix (getSymbolicLinkStatus, modificationTime)
import qualified Text.URI as URI

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
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
  { _thunkRev_commit :: Ref Ref.SHA1
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
  , _gitHubSource_private :: Bool
  }
  deriving (Show, Eq, Ord)

newtype GitUri = GitUri { unGitUri :: URI.URI } deriving (Eq, Ord, Show)

gitUriToText :: GitUri -> Text
gitUriToText (GitUri uri)
  | (T.toLower . URI.unRText <$> URI.uriScheme uri) == Just "file"
  , Just (_, path) <- URI.uriPath uri
  = "/" <> T.intercalate "/" (map URI.unRText $ NonEmpty.toList path)
  | otherwise = URI.render uri

data GitSource = GitSource
  { _gitSource_url :: GitUri
  , _gitSource_branch :: Maybe (Name Branch)
  , _gitSource_fetchSubmodules :: Bool
  , _gitSource_private :: Bool
  }
  deriving (Show, Eq, Ord)

newtype ThunkConfig = ThunkConfig
  { _thunkConfig_private :: Maybe Bool
  } deriving Show

data ThunkUpdateConfig = ThunkUpdateConfig
  { _thunkUpdateConfig_branch :: Maybe String
  , _thunkUpdateConfig_config :: ThunkConfig
  } deriving Show

data ThunkPackConfig = ThunkPackConfig
  { _thunkPackConfig_force :: Bool
  , _thunkPackConfig_config :: ThunkConfig
  } deriving Show

-- | Convert a GitHub source to a regular Git source. Assumes no submodules.
forgetGithub :: Bool -> GitHubSource -> GitSource
forgetGithub useSsh s = GitSource
  { _gitSource_url = GitUri $ URI.URI
    { URI.uriScheme = Just $ fromRight' $ URI.mkScheme $ if useSsh then "ssh" else "https"
    , URI.uriAuthority = Right $ URI.Authority
        { URI.authUserInfo = URI.UserInfo (fromRight' $ URI.mkUsername "git") Nothing
          <$ guard useSsh
        , URI.authHost = fromRight' $ URI.mkHost "github.com"
        , URI.authPort = Nothing
        }
    , URI.uriPath = Just ( False
                     , fromRight' . URI.mkPathPiece <$>
                       untagName (_gitHubSource_owner s)
                       :| [ untagName (_gitHubSource_repo s) <> ".git" ]
                     )
    , URI.uriQuery = []
    , URI.uriFragment = Nothing
    }
  , _gitSource_branch = _gitHubSource_branch s
  , _gitSource_fetchSubmodules = False
  , _gitSource_private = _gitHubSource_private s
  }

getThunkGitBranch :: ThunkPtr -> Maybe Text
getThunkGitBranch (ThunkPtr _ src) = fmap untagName $ case src of
  ThunkSource_GitHub s -> _gitHubSource_branch s
  ThunkSource_Git s -> _gitSource_branch s

commitNameToRef :: Name Commit -> Ref Ref.SHA1
commitNameToRef (N c) = Ref.fromHex $ encodeUtf8 c

-- TODO: Use spinner here.
getNixSha256ForUriUnpacked
  :: MonadObelisk m
  => GitUri
  -> m NixSha256
getNixSha256ForUriUnpacked uri =
  withExitFailMessage ("nix-prefetch-url: Failed to determine sha256 hash of URL " <> gitUriToText uri) $ do
    [hash] <- fmap T.lines $ readProcessAndLogOutput (Debug, Debug) $
      proc "nix-prefetch-url" ["--unpack", "--type", "sha256", T.unpack $ gitUriToText uri]
    pure hash

nixPrefetchGit :: MonadObelisk m => GitUri -> Text -> Bool -> m NixSha256
nixPrefetchGit uri rev fetchSubmodules =
  withExitFailMessage ("nix-prefetch-git: Failed to determine sha256 hash of Git repo " <> gitUriToText uri <> " at " <> rev) $ do
    out <- readProcessAndLogStderr Debug $
      proc "nix-prefetch-git" $ filter (/="")
        [ "--url", T.unpack $ gitUriToText uri
        , "--rev", T.unpack rev
        , if fetchSubmodules then "--fetch-submodules" else ""
        , "--quiet"
        ]

    case parseMaybe (Aeson..: "sha256") =<< Aeson.decodeStrict (encodeUtf8 out) of
      Nothing -> failWith $ "nix-prefetch-git: unrecognized output " <> out
      Just x -> pure x

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

attrCacheFileName :: FilePath
attrCacheFileName = ".attr-cache"

data ThunkType = ThunkType
  { _thunkType_loader :: Maybe FilePath
  , _thunkType_json :: FilePath
  , _thunkType_optional :: Set FilePath
  , _thunkType_loaderVersions :: NonEmpty Text
  , _thunkType_parser :: Aeson.Object -> Aeson.Parser ThunkPtr
  }

gitHubThunkType :: ThunkType
gitHubThunkType = ThunkType
  { _thunkType_loader = Just "default.nix"
  , _thunkType_json = "github.json"
  , _thunkType_optional = Set.fromList [attrCacheFileName]
  , _thunkType_loaderVersions = gitHubStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ \v ->
      ThunkSource_GitHub <$> parseGitHubSource v <|> ThunkSource_Git <$> parseGitSource v
  }

gitThunkType :: ThunkType
gitThunkType = ThunkType
  { _thunkType_loader = Just "default.nix"
  , _thunkType_json = "git.json"
  , _thunkType_optional = Set.fromList [attrCacheFileName]
  , _thunkType_loaderVersions = plainGitStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ fmap ThunkSource_Git . parseGitSource
  }

gitHubLoaderlessThunkType :: ThunkType
gitHubLoaderlessThunkType = ThunkType
  { _thunkType_loader = Nothing
  , _thunkType_json = "github.json"
  , _thunkType_optional = Set.fromList [attrCacheFileName]
  , _thunkType_loaderVersions = gitHubStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ \v ->
      ThunkSource_GitHub <$> parseGitHubSource v <|> ThunkSource_Git <$> parseGitSource v
  }

gitLoaderlessThunkType :: ThunkType
gitLoaderlessThunkType = ThunkType
  { _thunkType_loader = Nothing
  , _thunkType_json = "git.json"
  , _thunkType_optional = Set.fromList [attrCacheFileName]
  , _thunkType_loaderVersions = plainGitStandaloneLoaders
  , _thunkType_parser = parseThunkPtr $ fmap ThunkSource_Git . parseGitSource
  }

thunkTypes :: [ThunkType]
thunkTypes = [gitThunkType, gitLoaderlessThunkType, gitHubThunkType, gitHubLoaderlessThunkType]

findThunkType :: [ThunkType] -> FilePath -> IO (Either ReadThunkError ThunkType)
findThunkType types thunkDir = do
  matches <- fmap catMaybes $ forM types $ \thunkType -> do
    let
      expectedContents = Set.fromList $ (thunkDir </>) <$>
        maybeToList (_thunkType_loader thunkType) <> [ _thunkType_json thunkType ]
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

  case _thunkType_loader thunkType of
    Just loaderFile -> do
      -- Ensure that we recognize the thunk loader
      loader <- liftIO $ T.readFile $ thunkDir </> loaderFile
      unless (loader `elem` _thunkType_loaderVersions thunkType) $ do
        throwError $ ReadThunkError_UnrecognizedLoader loader
    Nothing -> pure ()

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
  private <- v Aeson..:? "private"
  pure $ GitHubSource
    { _gitHubSource_owner = owner
    , _gitHubSource_repo = repo
    , _gitHubSource_branch = branch
    , _gitHubSource_private = fromMaybe False private
    }

parseGitSource :: Aeson.Object -> Aeson.Parser GitSource
parseGitSource v = do
  Just url <- parseGitUri <$> v Aeson..: "url"
  branch <- v Aeson..:! "branch"
  fetchSubmodules <- v Aeson..:! "fetchSubmodules"
  private <- v Aeson..:? "private"
  pure $ GitSource
    { _gitSource_url = url
    , _gitSource_branch = branch
    , _gitSource_fetchSubmodules = fromMaybe False fetchSubmodules
    , _gitSource_private = fromMaybe False private
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
    , Just $ "private" .= _gitHubSource_private s
    ]
  ThunkSource_Git s -> encodePretty' plainGitCfg $ Aeson.object $ catMaybes
    [ Just $ "url" .= gitUriToText (_gitSource_url s)
    , Just $ "rev" .= Ref.toHexString (_thunkRev_commit rev)
    , ("branch" .=) <$> _gitSource_branch s
    , Just $ "sha256" .= _thunkRev_nixSha256 rev
    , Just $ "fetchSubmodules" .= _gitSource_fetchSubmodules s
    , Just $ "private" .= _gitSource_private s
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
        , "private"
        , "fetchSubmodules"
        ] <> compare
    , confTrailingNewline = True
    }

createThunk :: MonadIO m => FilePath -> ThunkPtr -> m ()
createThunk target thunk = liftIO $ do
  createDirectoryIfMissing True (target </> attrCacheFileName)
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

updateThunkToLatest :: MonadObelisk m => ThunkUpdateConfig -> FilePath -> m ()
updateThunkToLatest (ThunkUpdateConfig mBranch thunkConfig) target = withSpinner' ("Updating thunk " <> T.pack target <> " to latest") (pure $ const $ "Thunk " <> T.pack target <> " updated to latest") $ do
  checkThunkDirectory "ob thunk update directory cannot be '.'" target
  -- check to see if thunk should be updated to a specific branch or just update it's current branch
  case mBranch of
    Nothing -> do
      (overwrite, ptr) <- readThunk target >>= \case
        Left err -> failWith $ T.pack $ "thunk update: " <> show err
        Right c -> case c of
          ThunkData_Packed t -> return (target, t)
          ThunkData_Checkout _ -> failWith "cannot update an unpacked thunk"
      let src = _thunkPtr_source ptr
      rev <- getLatestRev src
      overwriteThunk overwrite $ modifyThunkPtrByConfig thunkConfig $ ThunkPtr
        { _thunkPtr_source = src
        , _thunkPtr_rev = rev
        }
    Just branch -> readThunk target >>= \case
      Left err -> failWith $ T.pack $ "thunk update: " <> show err
      Right c -> case c of
        ThunkData_Packed t -> case _thunkPtr_source t of
          ThunkSource_Git tsg -> setThunk thunkConfig target tsg branch
          ThunkSource_GitHub tsgh -> do
            let tsg = forgetGithub False tsgh
            setThunk thunkConfig target tsg branch
        ThunkData_Checkout _ -> failWith $ T.pack $ "thunk located at " <> show target <> " is unpacked. Use ob thunk pack on the desired directory and then try ob thunk update again."

setThunk :: MonadObelisk m => ThunkConfig -> FilePath -> GitSource -> String -> m ()
setThunk thunkConfig target gs branch = do
  newThunkPtr <- uriThunkPtr (_gitSource_url gs) (_thunkConfig_private thunkConfig) (Just $ T.pack branch) Nothing
  overwriteThunk target newThunkPtr
  updateThunkToLatest (ThunkUpdateConfig Nothing thunkConfig) target

-- | All recognized github standalone loaders, ordered from newest to oldest.
-- This tool will only ever produce the newest one when it writes a thunk.
gitHubStandaloneLoaders :: NonEmpty Text
gitHubStandaloneLoaders =
  gitHubStandaloneLoaderV4 :|
  [ gitHubStandaloneLoaderV3
  , gitHubStandaloneLoaderV2
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

gitHubStandaloneLoaderV4 :: Text
gitHubStandaloneLoaderV4 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let fetch = { private ? false, fetchSubmodules ? false, owner, repo, rev, sha256, ... }:"
  , "  if !fetchSubmodules && !private then builtins.fetchTarball {"
  , "    url = \"https://github.com/${owner}/${repo}/archive/${rev}.tar.gz\"; inherit sha256;"
  , "  } else (import <nixpkgs> {}).fetchFromGitHub {"
  , "    inherit owner repo rev sha256 fetchSubmodules private;"
  , "  };"
  , "in import (fetch (builtins.fromJSON (builtins.readFile ./github.json)))"
  ]

plainGitStandaloneLoaders :: NonEmpty Text
plainGitStandaloneLoaders =
  plainGitStandaloneLoaderV4 :|
  [ plainGitStandaloneLoaderV1
  , plainGitStandaloneLoaderV2
  , plainGitStandaloneLoaderV3
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

-- This loader has a bug because @builtins.fetchGit@ is not given a @ref@
-- and will fail to find commits without this because it does shallow clones.
plainGitStandaloneLoaderV3 :: Text
plainGitStandaloneLoaderV3 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let fetch = {url, rev, ref ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:"
  , "  let realUrl = let firstChar = builtins.substring 0 1 url; in"
  , "    if firstChar == \"/\" then /. + url"
  , "    else if firstChar == \".\" then ./. + url"
  , "    else url;"
  , "  in if !fetchSubmodules && private then builtins.fetchGit {"
  , "    url = realUrl; inherit rev;"
  , "  } else (import <nixpkgs> {}).fetchgit {"
  , "    url = realUrl; inherit rev sha256;"
  , "  };"
  , "in import (fetch (builtins.fromJSON (builtins.readFile ./git.json)))"
  ]

plainGitStandaloneLoaderV4 :: Text
plainGitStandaloneLoaderV4 = T.unlines
  [ "# DO NOT HAND-EDIT THIS FILE"
  , "let fetch = {url, rev, branch ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:"
  , "  let realUrl = let firstChar = builtins.substring 0 1 url; in"
  , "    if firstChar == \"/\" then /. + url"
  , "    else if firstChar == \".\" then ./. + url"
  , "    else url;"
  , "  in if !fetchSubmodules && private then builtins.fetchGit {"
  , "    url = realUrl; inherit rev;"
  , "    ${if branch == null then null else \"ref\"} = branch;"
  , "  } else (import <nixpkgs> {}).fetchgit {"
  , "    url = realUrl; inherit rev sha256;"
  , "  };"
  , "in import (fetch (builtins.fromJSON (builtins.readFile ./git.json)))"
  ]

thunkFileNames :: [FilePath]
thunkFileNames = ["default.nix", "github.json", "git.json"]

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
      cacheDir = thunkDir </> attrCacheFileName
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
      _ <- nixCmd $ NixCmd_Build $ def
        Lens.& nixBuildConfig_outLink Lens..~ OutLink_IndirectRoot cachePath
        Lens.& nixCmdConfig_target Lens..~ Target
          { _target_path = Just thunkDir
          , _target_attr = Just attr
          , _target_expr = Nothing
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
    _ -> nixCmd $ NixCmd_Build $ def
      Lens.& nixBuildConfig_outLink Lens..~ OutLink_None
      Lens.& nixCmdConfig_target Lens..~ Target
        { _target_path = Just exprPath
        , _target_attr = Just attr
        , _target_expr = Nothing
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
  updateThunkFromTmp p'
  return result
  where
    copyThunkToTmp tmpDir thunkDir = readThunk thunkDir >>= \case
      Left err -> failWith $ "withThunkUnpacked: " <> T.pack (show err)
      Right (ThunkData_Packed _) -> do
        let tmpThunk = tmpDir </> "thunk"
        callProcessAndLogOutput (Notice, Error) $
          proc cp ["-r", "-T", thunkDir, tmpThunk]
        return tmpThunk
      Right _ -> failWith "Thunk is not packed"
    updateThunkFromTmp p' = do
      _ <- packThunk' True (ThunkPackConfig False (ThunkConfig Nothing)) p'
      callProcessAndLogOutput (Notice, Error) $
        proc cp ["-r", "-T", p', p]

finalMsg :: Bool -> (a -> Text) -> Maybe (a -> Text)
finalMsg noTrail s = if noTrail then Nothing else Just s

unpackThunk :: MonadObelisk m => FilePath -> m ()
unpackThunk = unpackThunk' False

-- | Check that we are not somewhere inside the thunk directory
checkThunkDirectory :: MonadObelisk m => Text -> FilePath -> m ()
checkThunkDirectory msg thunkDir = do
  currentDir <- liftIO getCurrentDirectory
  thunkDir' <- liftIO $ canonicalizePath thunkDir
  when (thunkDir' `L.isInfixOf` currentDir) $ failWith msg

unpackThunk' :: MonadObelisk m => Bool -> FilePath -> m ()
unpackThunk' noTrail thunkDir = checkThunkDirectory "Can't pack/unpack from within the thunk directory" thunkDir >> readThunk thunkDir >>= \case
  Left err -> failWith $ "thunk unpack: " <> T.pack (show err)
  --TODO: Overwrite option that rechecks out thunk; force option to do so even if working directory is dirty
  Right (ThunkData_Checkout _) -> failWith "thunk unpack: thunk is already unpacked"
  Right (ThunkData_Packed tptr) -> do
    let (thunkParent, thunkName) = splitFileName thunkDir
    withTempDirectory thunkParent thunkName $ \tmpRepo -> do
      let obGitDir = tmpRepo </> ".git" </> "obelisk"
          s = case _thunkPtr_source tptr of
            ThunkSource_GitHub s' -> forgetGithub False s'
            ThunkSource_Git s' -> s'
      withSpinner' ("Fetching thunk " <> T.pack thunkName)
                   (finalMsg noTrail $ const $ "Fetched thunk " <> T.pack thunkName) $ do
        let git = callProcessAndLogOutput (Notice, Notice) . gitProc tmpRepo
        git $ [ "clone" ]
          ++  ("--recursive" <$ guard (_gitSource_fetchSubmodules s))
          ++  [ T.unpack $ gitUriToText $ _gitSource_url s ]
          ++  do branch <- maybeToList $ _gitSource_branch s
                 [ "--branch", T.unpack $ untagName branch ]
        git ["reset", "--hard", Ref.toHexString $ _thunkRev_commit $ _thunkPtr_rev tptr]
        when (_gitSource_fetchSubmodules s) $
          git ["submodule", "update", "--recursive", "--init"]

        liftIO $ createDirectory obGitDir
        callProcessAndLogOutput (Notice, Error) $
          proc cp ["-r", "-T", thunkDir </> ".", obGitDir </> "orig-thunk"]
        callProcessAndLogOutput (Notice, Error) $
          proc "rm" ["-r", thunkDir]
        callProcessAndLogOutput (Notice, Error) $
          proc "mv" ["-T", tmpRepo, thunkDir]

--TODO: add a rollback mode to pack to the original thunk
packThunk :: MonadObelisk m => ThunkPackConfig -> FilePath -> m ThunkPtr
packThunk = packThunk' False

packThunk' :: MonadObelisk m => Bool -> ThunkPackConfig -> FilePath -> m ThunkPtr
packThunk' noTrail (ThunkPackConfig force thunkConfig) thunkDir = checkThunkDirectory "Can't pack/unpack from within the thunk directory" thunkDir >> readThunk thunkDir >>= \case
  Left err -> failWith $ T.pack $ "thunk pack: " <> show err
  Right (ThunkData_Packed _) -> failWith "pack: thunk is already packed"
  Right (ThunkData_Checkout _) -> do
    withSpinner' ("Packing thunk " <> T.pack thunkDir)
                 (finalMsg noTrail $ const $ "Packed thunk " <> T.pack thunkDir) $ do
      thunkPtr <- modifyThunkPtrByConfig thunkConfig <$> getThunkPtr (not force) thunkDir (_thunkConfig_private thunkConfig)
      callProcessAndLogOutput (Debug, Error) $ proc "rm" ["-rf", thunkDir]
      liftIO $ createThunk thunkDir thunkPtr
      pure thunkPtr

modifyThunkPtrByConfig :: ThunkConfig -> ThunkPtr -> ThunkPtr
modifyThunkPtrByConfig (ThunkConfig markPrivate') ptr = case markPrivate' of
  Nothing -> ptr
  Just markPrivate -> ptr { _thunkPtr_source = case _thunkPtr_source ptr of
      ThunkSource_Git s -> ThunkSource_Git $ s { _gitSource_private = markPrivate }
      ThunkSource_GitHub s -> ThunkSource_GitHub $ s { _gitHubSource_private = markPrivate }
    }

getThunkPtr :: forall m. MonadObelisk m => Bool -> FilePath -> Maybe Bool -> m ThunkPtr
getThunkPtr checkClean thunkDir mPrivate = do
  when checkClean $ ensureCleanGitRepo thunkDir True
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

  -- Get current branch
  (mCurrentBranch, mCurrentCommit) <- do
    b <- listToMaybe
      <$> T.lines
      <$> readGitProcess thunkDir ["rev-parse", "--abbrev-ref", "HEAD"]
    c <- listToMaybe
      <$> T.lines
      <$> readGitProcess thunkDir ["rev-parse", "HEAD"]
    case b of
      (Just "HEAD") -> failWith $ T.unlines
        [ "thunk pack: You are in 'detached HEAD' state."
        , "If you want to pack at the current ref \
          \then please create a new branch with 'git checkout -b <new-branch-name>' and push this upstream."
        ]
      _ -> return (b, c)

  -- Get information on all branches and their (optional) designated upstream
  -- correspondents
  (headDump :: [Text]) <- T.lines <$> readGitProcess thunkDir
    [ "for-each-ref"
    , "--format=%(refname:short) %(upstream:short) %(upstream:remotename)"
    , "refs/heads/"
    ]

  (headInfo :: Map Text (Maybe (Text, Text)))
    <- fmap Map.fromList $ forM headDump $ \line -> do
      (branch : restOfLine) <- pure $ T.words line
      mUpstream <- case restOfLine of
        [] -> pure Nothing
        [u, r] -> pure $ Just (u, r)
        (_:_) -> failWith "git for-each-ref invalid output"
      pure (branch, mUpstream)

  putLog Debug $ "branches: " <> T.pack (show headInfo)

  let errorMap :: Map Text ()
      headUpstream :: Map Text (Text, Text)
      (errorMap, headUpstream) = flip Map.mapEither headInfo $ \case
        Nothing -> Left ()
        Just b -> Right b

  putLog Debug $ "branches with upstream branch set: " <> T.pack (show headUpstream)

  -- Check that every branch has a remote equivalent
  when checkClean $ do
    let untrackedBranches = Map.keys errorMap
    when (not $ L.null untrackedBranches) $ failWith $ T.unlines $
      [ "thunk pack: Certain branches in the thunk have no upstream branch \
        \set. This means we don't know to check whether all your work is \
        \saved. The offending branches are:"
      , ""
      , T.unwords untrackedBranches
      , ""
      , "To fix this, you probably want to do:"
      , ""
      ] ++
      ((\branch -> "git push -u origin " <> branch) <$> untrackedBranches) ++
      [ ""
      , "These will push the branches to the default remote under the same \
        \name, and (thanks to the `-u`) remember that choice so you don't \
        \get this error again."
      ]

    -- loosely by https://stackoverflow.com/questions/7773939/show-git-ahead-and-behind-info-for-all-branches-including-remotes
    stats <- iforM headUpstream $ \branch (upstream, _remote) -> do
      (stat :: [Text]) <- T.lines <$> readGitProcess thunkDir
        [ "rev-list", "--left-right"
        , T.unpack branch <> "..." <> T.unpack upstream
        ]
      let ahead = length $ [ () | Just ('<', _) <- T.uncons <$> stat ]
          behind = length $ [ () | Just ('>', _) <- T.uncons <$> stat ]
      pure (upstream, (ahead, behind))

    -- Those branches which have commits ahead of, i.e. not on, the upstream
    -- branch. Purely being behind is fine.
    let nonGood = Map.filter ((/= 0) . fst . snd) stats

    when (not $ Map.null nonGood) $ failWith $ T.unlines $
      [ "thunk pack: Certain branches in the thunk have commits not yet pushed upstream:"
      , ""
      ] ++
      flip map (Map.toList nonGood) (\(branch, (upstream, (ahead, behind))) -> mconcat
        ["  ", branch, " ahead: ", T.pack (show ahead), " behind: ", T.pack (show behind), " remote branch ", upstream]) ++
      [ ""
      , "Please push these upstream and try again. (Or just fetch, if they are somehow \
        \pushed but this repo's remote tracking branches don't know it.)"
      ]

  -- We assume it's safe to pack the thunk at this point
  putLog Informational "All changes safe in git remotes. OK to pack thunk."

  let remote = maybe "origin" snd $ flip Map.lookup headUpstream =<< mCurrentBranch

  [remoteUri'] <- fmap T.lines $ readGitProcess thunkDir
    [ "config"
    , "--get"
    , "remote." <> T.unpack remote <> ".url"
    ]

  remoteUri <- case parseGitUri remoteUri' of
    Nothing -> failWith $ "Could not identify git remote: " <> remoteUri'
    Just uri -> pure uri
  uriThunkPtr remoteUri mPrivate mCurrentBranch mCurrentCommit

-- | Get the latest revision available from the given source
getLatestRev :: MonadObelisk m => ThunkSource -> m ThunkRev
getLatestRev os = do
  let gitS = case os of
        ThunkSource_GitHub s -> forgetGithub False s
        ThunkSource_Git s -> s
  (_, commit) <- gitGetCommitBranch (_gitSource_url gitS) (untagName <$> _gitSource_branch gitS)
  case os of
    ThunkSource_GitHub s -> githubThunkRev s commit
    ThunkSource_Git s -> gitThunkRev s commit

-- | Convert a URI to a thunk
--
-- If the URL is a github URL, we try to just download an archive for
-- performance. If that doesn't work (e.g. authentication issue), we fall back
-- on just doing things the normal way for git repos in general, and save it as
-- a regular git thunk.
uriThunkPtr :: MonadObelisk m => GitUri -> Maybe Bool -> Maybe Text -> Maybe Text -> m ThunkPtr
uriThunkPtr uri mPrivate mbranch mcommit = do
  commit <- case mcommit of
    Nothing -> gitGetCommitBranch uri mbranch >>= return . snd
    (Just c) -> return c
  (src, rev) <- uriToThunkSource uri mPrivate mbranch >>= \case
    ThunkSource_GitHub s -> do
      rev <- runExceptT $ githubThunkRev s commit
      case rev of
        Right r -> pure (ThunkSource_GitHub s, r)
        Left e -> do
          putLog Warning "\
\Failed to fetch archive from GitHub. This is probably a private repo. \
\Falling back on normal fetchgit. Original failure:"
          errorToWarning e
          let s' = forgetGithub True s
          (,) (ThunkSource_Git s') <$> gitThunkRev s' commit
    ThunkSource_Git s -> (,) (ThunkSource_Git s) <$> gitThunkRev s commit
  pure $ ThunkPtr
    { _thunkPtr_rev = rev
    , _thunkPtr_source = src
    }

-- | N.B. Cannot infer all fields.
--
-- If the thunk is a GitHub thunk and fails, we do *not* fall back like with
-- `uriThunkPtr`. Unlike a plain URL, a thunk src explicitly states which method
-- should be employed, and so we respect that.
uriToThunkSource :: MonadObelisk m => GitUri -> Maybe Bool -> Maybe Text -> m ThunkSource
uriToThunkSource (GitUri u) mPrivate
  | Right uriAuth <- URI.uriAuthority u
  , Just scheme <- URI.unRText <$> URI.uriScheme u
  , case scheme of
      "ssh" -> uriAuth == URI.Authority
        { URI.authUserInfo = Just $ URI.UserInfo (fromRight' $ URI.mkUsername "git") Nothing
        , URI.authHost = fromRight' $ URI.mkHost "github.com"
        , URI.authPort = Nothing
        }
      s -> s `L.elem` [ "git", "https", "http" ] -- "http:" just redirects to "https:"
        && URI.unRText (URI.authHost uriAuth) == "github.com"
  , Just (_, owner :| [repoish]) <- URI.uriPath u
  = \mbranch -> do
      isPrivate <- getIsPrivate
      pure $ ThunkSource_GitHub $ GitHubSource
        { _gitHubSource_owner = N $ URI.unRText owner
        , _gitHubSource_repo = N $ let
            repoish' = URI.unRText repoish
          in fromMaybe repoish' $ T.stripSuffix ".git" repoish'
        , _gitHubSource_branch = N <$> mbranch
        , _gitHubSource_private = isPrivate
        }

  | otherwise = \mbranch -> do
      isPrivate <- getIsPrivate
      pure $ ThunkSource_Git $ GitSource
        { _gitSource_url = GitUri u
        , _gitSource_branch = N <$> mbranch
        , _gitSource_fetchSubmodules = False -- TODO: How do we determine if this should be true?
        , _gitSource_private = isPrivate
        }
  where
    getIsPrivate = maybe (guessGitRepoIsPrivate $ GitUri u) pure mPrivate

guessGitRepoIsPrivate :: MonadObelisk m => GitUri -> m Bool
guessGitRepoIsPrivate uri = flip fix urisToTry $ \loop -> \case
  [] -> pure True
  uriAttempt:xs -> do
    result <- readCreateProcessWithExitCode $
      isolateGitProc $
        gitProcNoRepo
          [ "ls-remote"
          , "--quiet"
          , "--exit-code"
          , "--symref"
          , T.unpack $ gitUriToText uriAttempt
          ]
    case result of
      (ExitSuccess, _, _) -> pure False -- Must be a public repo
      _ -> loop xs
  where
    urisToTry = nubOrd $
      -- Include the original URI if it isn't using SSH because SSH will certainly fail.
      [uri | fmap URI.unRText (URI.uriScheme (unGitUri uri)) /= Just "ssh"] <>
      [changeScheme "https" uri, changeScheme "http" uri]
    changeScheme scheme (GitUri u) = GitUri $ u
      { URI.uriScheme = URI.mkScheme scheme
      , URI.uriAuthority = (\x -> x { URI.authUserInfo = Nothing }) <$> URI.uriAuthority u
      }

-- Funny signature indicates no effects depend on the optional branch name.
githubThunkRev
  :: forall m
  .  MonadObelisk m
  => GitHubSource
  -> Text
  -> m ThunkRev
githubThunkRev s commit = do
  owner <- forcePP $ _gitHubSource_owner s
  repo <- forcePP $ _gitHubSource_repo s
  revTarball <- URI.mkPathPiece $ commit <> ".tar.gz"
  let archiveUri = GitUri $ URI.URI
        { URI.uriScheme = Just $ fromRight' $ URI.mkScheme "https"
        , URI.uriAuthority = Right $ URI.Authority
          { URI.authUserInfo = Nothing
          , URI.authHost = fromRight' $ URI.mkHost "github.com"
          , URI.authPort = Nothing
          }
        , URI.uriPath = Just ( False
                         , owner :| [ repo, fromRight' $ URI.mkPathPiece "archive", revTarball ]
                     )
    , URI.uriQuery = []
    , URI.uriFragment = Nothing
    }
  hash <- getNixSha256ForUriUnpacked archiveUri
  putLog Debug $ "Nix sha256 is " <> hash
  return $ ThunkRev
    { _thunkRev_commit = commitNameToRef $ N commit
    , _thunkRev_nixSha256 = hash
    }
  where
    forcePP :: Name entity -> m (URI.RText 'URI.PathPiece)
    forcePP = URI.mkPathPiece . untagName

gitThunkRev
  :: MonadObelisk m
  => GitSource
  -> Text
  -> m ThunkRev
gitThunkRev s commit = do
  let u = _gitSource_url s
      protocols = ["file", "https", "ssh", "git"]
      scheme = maybe "file" URI.unRText $ URI.uriScheme $ (\(GitUri x) -> x) u
  unless (T.toLower scheme `elem` protocols) $
    failWith $ "obelisk currently only supports "
      <> T.intercalate ", " protocols <> " protocols for plain Git remotes"
  hash <- nixPrefetchGit u commit $ _gitSource_fetchSubmodules s
  putLog Informational $ "Nix sha256 is " <> hash
  pure $ ThunkRev
    { _thunkRev_commit = commitNameToRef (N commit)
    , _thunkRev_nixSha256 = hash
    }

-- | Given the URI to a git remote, and an optional branch name, return the name
-- of the branch along with the hash of the commit at tip of that branch.
--
-- If the branch name is passed in, it is returned exactly as-is. If it is not
-- passed it, the default branch of the repo is used instead.

gitGetCommitBranch
  :: MonadObelisk m => GitUri -> Maybe Text -> m (Text, CommitId)
gitGetCommitBranch uri mbranch = withExitFailMessage ("Failure for git remote " <> uriMsg) $ do
  (_, bothMaps) <- gitLsRemote
    (T.unpack $ gitUriToText uri)
    (GitRef_Branch <$> mbranch)
    Nothing
  branch <- case mbranch of
    Nothing -> withExitFailMessage "Failed to find default branch" $ do
      b <- rethrowE $ gitLookupDefaultBranch bothMaps
      putLog Debug $ "Default branch for remote repo " <> uriMsg <> " is " <> b
      pure b
    Just b -> pure b
  commit <- rethrowE $ gitLookupCommitForRef bothMaps (GitRef_Branch branch)
  putLog Informational $ "Latest commit in branch " <> branch
    <> " from remote repo " <> uriMsg
    <> " is " <> commit
  pure (branch, commit)
  where
    rethrowE = either failWith pure
    uriMsg = gitUriToText uri

parseGitUri :: Text -> Maybe GitUri
parseGitUri x = GitUri <$> (parseFileURI x <|> parseAbsoluteURI x <|> parseSshShorthand x)

parseFileURI :: Text -> Maybe URI.URI
parseFileURI uri = if "/" `T.isPrefixOf` uri then parseAbsoluteURI ("file://" <> uri) else Nothing

parseAbsoluteURI :: Text -> Maybe URI.URI
parseAbsoluteURI uri = do
  parsedUri <- URI.mkURI uri
  guard $ URI.isPathAbsolute parsedUri
  pure parsedUri

parseSshShorthand :: Text -> Maybe URI.URI
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
  URI.mkURI properUri
