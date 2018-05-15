{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Obelisk.Command.Thunk
  ( ThunkPtr (..)
  , ThunkRev (..)
  , ThunkSource (..)
  , ThunkData (..)
  , GitHubSource (..)
  , getLatestRev
  , updateThunkToLatest
  , createThunk
  , createThunkWithLatest
  , nixBuildAttrWithCache
  , nixBuildThunkAttrWithCache
  , unpackThunk
  , packThunk
  , readThunk
  , getThunkPtr
  ) where

import Control.Applicative
import Control.Exception (displayException, throwIO)
import Control.Monad
import Control.Monad.Catch (handle)
import Control.Monad.Except
import Data.Aeson (decode, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Foldable
import Data.Git.Ref (Ref)
import qualified Data.Git.Ref as Ref
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
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
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Posix (getSymbolicLinkStatus, modificationTime)
import System.Process (StdStream (CreatePipe), callProcess, createProcess, proc, readProcess, std_err,
                       std_out, waitForProcess)

import Development.Placeholders
import Obelisk.App (MonadObelisk)
import Obelisk.CLI (Severity (..), callProcessAndLogOutput, failWith, putLog, withSpinner)
import Obelisk.Command.Utils (checkGitCleanStatus, cp, procWithPackages)

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
  , _gitHubSource_private :: Bool
  }
  deriving (Show, Eq, Ord)

data GitSource = GitSource
  { _gitSource_url :: URI
  --TODO: Is it ok to use the types from the GitHub api bindings?
  , _gitSource_branch :: Maybe (Name Branch)
  , _gitSource_fetchSubmodules :: Bool
  }
  deriving (Show, Eq, Ord)

commitNameToRef :: Name Commit -> Ref
commitNameToRef (N c) = Ref.fromHex $ encodeUtf8 c

-- TODO: Use spinner here.
getNixSha256ForUriUnpacked :: URI -> IO NixSha256
getNixSha256ForUriUnpacked uri = do
  --TODO: Make this package depend on nix-prefetch-url properly
  let cmd = proc "nix-prefetch-url" ["--unpack" , "--type" , "sha256" , show uri]
  (_, Just out, Just err, p) <- createProcess cmd
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  --TODO: Deal with errors here; usually they're HTTP errors
  waitForProcess p >>= \case
    ExitSuccess -> return ()
    _ -> do
     hPutStrLn stdout =<< hGetContents out
     hPutStrLn stderr =<< hGetContents err
     --TODO: actual error
     fail "nix-prefetch-url"
  T.strip <$> T.hGetContents out

-- | Get the latest revision available from the given source
getLatestRev :: ThunkSource -> IO ThunkRev
getLatestRev = \case
  ThunkSource_GitHub s -> do
    auth <- getHubAuth "github.com"
    commitsResult <- executeRequestMaybe auth $ commitsWithOptionsForR
      (_gitHubSource_owner s)
      (_gitHubSource_repo s)
      (FetchAtLeast 1)
      $ case _gitHubSource_branch s of
          Nothing -> []
          Just b -> [CommitQuerySha $ untagName b]
    commitInfos <- either throwIO return commitsResult
    commitInfo : _ <- return $ toList commitInfos
    let commit = commitSha commitInfo
    Right archiveUri <- executeRequestMaybe auth $ archiveForR (_gitHubSource_owner s) (_gitHubSource_repo s) ArchiveFormatTarball $ Just $ untagName commit
    nixSha256 <- getNixSha256ForUriUnpacked archiveUri
    return $ ThunkRev
      { _thunkRev_commit = commitNameToRef commit
      , _thunkRev_nixSha256 = nixSha256
      }
  ThunkSource_Git _s -> do
    $notImplemented

--TODO: Pretty print these
data ReadThunkError
   = ReadThunkError_WrongContents (Set String) (Set String)
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

-- | Read a thunk and validate that it is exactly a packed thunk.
-- If additional data is present, fail.
readPackedThunk :: FilePath -> IO (Either ReadThunkError ThunkPtr)
readPackedThunk thunkDir = runExceptT $ do
  let thunkLoader = thunkDir </> "default.nix"
      githubJson = thunkDir </> "github.json"
      attrCache = thunkDir </> ".attr-cache"
      expectedContents = Set.fromList $
        [ githubJson
        , thunkLoader
        ]
      optionalContents = Set.fromList $
        [ attrCache
        ]

  -- Ensure that there aren't any other files in the thunk
  -- NB: System.Directory.listDirectory returns the contents without the directory path
  files <- liftIO $ Set.fromList . fmap (thunkDir </>) <$> listDirectory thunkDir
  let unexpectedContents = files `Set.difference` (expectedContents `Set.union` optionalContents)
      missingContents = expectedContents `Set.difference` files
  unless (Set.null unexpectedContents && Set.null missingContents) $ do
    throwError $ ReadThunkError_WrongContents unexpectedContents missingContents

  -- Ensure that we recognize the thunk loader
  loader <- liftIO $ T.readFile thunkLoader
  unless (loader `elem` gitHubStandaloneLoaders) $ do
    throwError $ ReadThunkError_UnrecognizedLoader loader

  txt <- liftIO $ LBS.readFile githubJson
  let parseThunk v = do
        rev <- v Aeson..: "rev"
        sha256 <- v Aeson..: "sha256"
        src <- parseGitHubSource v
           <|> parseGitSource v
        return $ ThunkPtr
          { _thunkPtr_rev = ThunkRev
            { _thunkRev_commit = Ref.fromHexString rev
            , _thunkRev_nixSha256 = sha256
            }
          , _thunkPtr_source = src
          }
      parseGitHubSource v = do
        owner <- v Aeson..: "owner"
        repo <- v Aeson..: "repo"
        branch <- v Aeson..:! "branch"
        mPrivate <- v Aeson..:! "private"
        return $ ThunkSource_GitHub $ GitHubSource
          { _gitHubSource_owner = owner
          , _gitHubSource_repo = repo
          , _gitHubSource_branch = branch
          , _gitHubSource_private = fromMaybe False mPrivate
          }
      parseGitSource v = do
        Just url <- parseURI <$> v Aeson..: "url"
        branch <- v Aeson..:! "branch"
        fetchSubmodules <- v Aeson..: "fetchSubmodules"
        return $ ThunkSource_Git $ GitSource
          { _gitSource_url = url
          , _gitSource_branch = branch
          , _gitSource_fetchSubmodules = fetchSubmodules
          }
  case parseMaybe parseThunk =<< decode txt of
    Nothing -> throwError $ ReadThunkError_UnparseablePtr txt
    Just ptr -> return ptr

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
    , case _gitHubSource_branch s of
        Just b -> Just $ "branch" .= b
        Nothing -> Nothing
    , if _gitHubSource_private s
      then Just $ "private" .= True
      else Nothing
    , Just $ "rev" .= Ref.toHexString (_thunkRev_commit rev)
    , Just $ "sha256" .= _thunkRev_nixSha256 rev
    ]
  ThunkSource_Git s -> encodePretty' plainGitCfg $ Aeson.object $ catMaybes
    --TODO: Is this safe? read/show is jank for `URI`
    [ Just $ "url" .= show (_gitSource_url s)
    , Just $ "rev" .= Ref.toHexString (_thunkRev_commit rev)
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

createThunk :: FilePath -> ThunkPtr -> IO ()
createThunk target thunk = do
  createDirectoryIfMissing True (target </> ".attr-cache")
  T.writeFile (target </> "default.nix") (thunkPtrLoader thunk)
  LBS.writeFile (target </> "github.json") (encodeThunkPtrData thunk)

createThunkWithLatest :: FilePath -> ThunkSource -> IO ()
createThunkWithLatest target s = do
  rev <- getLatestRev s
  createThunk target $ ThunkPtr
    { _thunkPtr_source = s
    , _thunkPtr_rev = rev
    }

updateThunkToLatest :: MonadObelisk m => FilePath -> m ()
updateThunkToLatest target = do
  (overwrite, ptr) <- readThunk target >>= \case
    Left err -> failWith $ T.pack $ "thunk update: " <> show err
    Right c -> case c of
      ThunkData_Packed t -> return (target, t)
      ThunkData_Checkout _ -> failWith "cannot update an unpacked thunk"
  let src = _thunkPtr_source ptr
  rev <- liftIO $ getLatestRev src
  overwriteThunk overwrite $ ThunkPtr
    { _thunkPtr_source = src
    , _thunkPtr_rev = rev
    }

-- | All recognized github standalone loaders, ordered from newest to oldest.
-- This tool will only ever produce the newest one when it writes a thunk.
gitHubStandaloneLoaders :: NonEmpty Text
gitHubStandaloneLoaders =
  gitHubStandaloneLoaderV2 :|
  [ gitHubStandaloneLoaderV1
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

plainGitStandaloneLoaders :: NonEmpty Text
plainGitStandaloneLoaders =
  plainGitStandaloneLoaderV1 :|
  [
  ]

plainGitStandaloneLoaderV1 :: Text
plainGitStandaloneLoaderV1 = T.unlines
  [ $notImplemented
  ]

-- | Look for authentication info from the 'hub' command
getHubAuth
  :: Text -- ^ Domain name
  -> IO (Maybe Auth)
getHubAuth domain = do
  hubConfig <- getXdgDirectory XdgConfig "hub"
  Yaml.decodeFile hubConfig >>= \case
    Nothing -> return Nothing
    Just v -> return $ flip parseMaybe v $ \v' -> do
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
  liftIO $ createDirectoryIfMissing False cacheDir
  latestChange <- liftIO $ maximum <$> mapM (getModificationTime . (thunkDir </>)) ["default.nix", "github.json"]
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

unpackThunk
  :: MonadObelisk m
  => FilePath
  -> m ()
unpackThunk thunkDir = readThunk thunkDir >>= \case
  Left err -> failWith $ "thunk unpack: " <> (T.pack $ show err)
  --TODO: Overwrite option that rechecks out thunk; force option to do so even if working directory is dirty
  Right (ThunkData_Checkout _) -> failWith "thunk unpack: thunk is already unpacked"
  Right (ThunkData_Packed tptr) -> case _thunkPtr_source tptr of
    ThunkSource_GitHub s | (thunkParent, thunkName) <- splitFileName thunkDir -> withTempDirectory thunkParent thunkName $ \tmpRepo -> do
      mauth <- liftIO $ getHubAuth "github.com"
      repoResult <- liftIO $ executeRequestMaybe mauth $ repositoryR (_gitHubSource_owner s) (_gitHubSource_repo s)
      githubURI <- liftIO $ either throwIO (return . repoSshUrl) repoResult >>= \case
        Nothing -> fail "Cannot determine clone URI for thunk source"
        Just c -> return $ T.unpack $ getUrl c
      withSpinner ("Retrieving thunk " <> T.pack thunkName <> " from GitHub") $ do
        callProcessAndLogOutput (Notice, Notice) $
          proc "hub" ["clone", "-n", githubURI, tmpRepo]
        let obGitDir = tmpRepo </> ".git" </> "obelisk"
        --If this directory already exists then something is weird and we should fail
        liftIO $ createDirectory obGitDir
        callProcessAndLogOutput (Notice, Error) $
          cp ["-r", "-T", thunkDir </> ".", obGitDir </> "orig-thunk"]
      withSpinner ("Preparing thunk in " <> T.pack thunkDir) $ do
        -- Checkout
        putLog Notice $ "Checking out " <> T.pack (show $ maybe "" untagName $ _gitHubSource_branch s)
        callProcessAndLogOutput (Notice, Notice) $
          proc "hub" $ concat
            [ ["-C", tmpRepo]
            , pure "checkout"
            , maybe [] (\n -> ["-B", T.unpack (untagName n)]) (_gitHubSource_branch s)
            , pure $ Ref.toHexString $ _thunkRev_commit $ _thunkPtr_rev tptr
            ]
        -- Set upstream branch
        forM_ (_gitHubSource_branch s) $ \branch ->
          callProcessAndLogOutput (Notice, Error) $
            proc "hub" ["-C", tmpRepo, "branch", "-u", "origin/" <> T.unpack (untagName branch)]
        callProcessAndLogOutput (Notice, Error) $
          proc "rm" ["-r", thunkDir]
        callProcessAndLogOutput (Notice, Error) $
          procWithPackages ["coreutils"] "mv"
            [ "-T"
            , tmpRepo
            , thunkDir
            ]
    ThunkSource_Git _s -> do
      $notImplemented

--TODO: add force mode to pack even if changes are present
--TODO: add a rollback mode to pack to the original thunk
packThunk
  :: MonadObelisk m
  => FilePath
  -> String
  -> m ()
packThunk thunkDir upstream = readThunk thunkDir >>= \case
  Left err -> failWith $ T.pack $ "thunk pack: " <> show err
  Right (ThunkData_Packed _) -> failWith "pack: thunk is already packed"
  Right (ThunkData_Checkout _) -> do
    thunkPtr <- getThunkPtr thunkDir upstream
    liftIO $ callProcess "rm"
      [ "-rf"
      , thunkDir
      ]
    withSpinner ("Packing thunk " <> T.pack thunkDir) $
      liftIO $ createThunk thunkDir thunkPtr

getThunkPtr :: MonadObelisk m => FilePath -> String -> m ThunkPtr
getThunkPtr thunkDir upstream = do
    liftIO (checkGitCleanStatus thunkDir) >>= \case
      False -> do
        statusDebug <- liftIO $ fmap (T.strip . T.pack) $ readProcess "hub"
          [ "-C", thunkDir, "status", "--ignored" ] ""
        putLog Warning "cannot proceed with unsaved working copy (git status):"
        putLog Notice statusDebug
        failWith "thunk pack: thunk checkout contains unsaved modifications"
      True -> return ()

    -- Check whether there are any stashes
    stashOutput <- liftIO $ fmap T.pack $ readProcess "hub" [ "-C", thunkDir, "stash", "list" ] ""
    case T.null stashOutput of
      False -> do
        failWith $ T.unlines $
          [ "thunk pack: thunk checkout has stashes"
          , "git stash list:"
          ] ++ T.lines stashOutput
      True -> return ()

    --Check whether all local heads are pushed
    repoHeads <- liftIO $ lines <$> readProcess "hub"
      [ "-C", thunkDir
      , "for-each-ref", "--format=%(refname:short)", "refs/heads/"
      ] ""
    remotes <- liftIO $ lines <$> readProcess "hub" [ "-C", thunkDir, "remote" ] ""
    --Check that the upstream specified actually exists
    case L.find (== upstream) remotes of
      Nothing -> failWith $ T.pack $ "thunk pack: upstream " <> upstream <> " does not exist. Available upstreams are " <> L.intercalate ", " remotes
      Just _ -> return ()
    -- iterate over cartesian product
    forM_ repoHeads $ \hd -> do
      [localRev] <- liftIO $ lines <$> readProcess "hub" [ "-C", thunkDir, "rev-parse", hd ] ""
      forM_ remotes $ \rm -> do
        --TODO: The error you get if a branch isn't pushed anywhere could be made more user friendly
        [remoteMergeRev] <- liftIO $ lines <$> readProcess "hub"
          [ "-C", thunkDir
          , "merge-base"
          , localRev
          , "remotes/" <> rm <> "/" <> hd
          ] ""
        case remoteMergeRev == localRev of
          False -> failWith $ T.pack $ mconcat [ "thunk unpack: branch ", hd, " has not been pushed to ", rm ]
          True -> return ()

    --We assume it's safe to pack the thunk at this point
    [remoteUri'] <- liftIO $ lines <$> readProcess "hub"
       [ "-C", thunkDir
       , "config", "--get", "remote." <> upstream <> ".url"
       ] ""

    let uriParseFailure = "Could not identify git remote: " <> remoteUri'
        mRemoteUri = parseURIReference remoteUri'
                <|> parseSshShorthand remoteUri'
    case mRemoteUri of
      Nothing -> failWith $ T.pack uriParseFailure
      Just remoteUri -> liftIO $ do
        refs <- fmap (refsToTuples . words) . lines <$> readProcess "hub"
          [ "-C"
          , thunkDir
          , "show-ref"
          , "--head" ] ""
        -- safe because exactly one hash is associated with HEAD
        let Just currentHead = fst <$> L.find ((== "HEAD") . snd) refs
            remoteHeads = Map.fromList $ catMaybes $ fmap
              (\(x,y) -> (,) x <$> L.stripPrefix ("refs/remotes/" <> upstream <> "/") y)
              refs
            localHeads = Map.fromList $ catMaybes $ fmap
              (\(x,y) -> (,) x <$> L.stripPrefix "refs/heads/" y)
              refs
            currentUpstreamBranch = Map.lookup currentHead remoteHeads
                                <|> Map.lookup currentHead localHeads
        if
          | isGithubThunk remoteUri -> githubThunkPtr remoteUri currentHead currentUpstreamBranch
          | otherwise -> gitThunkPtr remoteUri currentHead currentUpstreamBranch
 where
  refsToTuples [x, y] = (x, y)
  refsToTuples x = error $ "thunk pack: cannot parse ref " <> show x
  isGithubThunk u
    | Just uriAuth <- uriAuthority u
    = case uriScheme u of
         "ssh:" -> uriAuth == URIAuth "git@" "github.com" ""
         s -> s `L.elem` [ "git:", "https:", "http:" ] -- "http:" just redirects to "https:"
           && uriRegName uriAuth == "github.com"
    | otherwise = False
  githubThunkPtr u commit' branch' = do
    ["/", owner', repo'] <- return $ splitDirectories (uriPath u)
    let owner = N (T.pack owner')
        repo = N (T.pack (dropExtension repo'))
        commit = N (T.pack commit')
        branch = N . T.pack <$> branch'
    mauth <- getHubAuth "github.com"
    repoResult <- executeRequestMaybe mauth $ repositoryR owner repo
    repoIsPrivate <- either throwIO (return . repoPrivate) repoResult
    archiveResult <- executeRequestMaybe mauth $
      archiveForR owner repo ArchiveFormatTarball (Just (untagName commit))
    archiveUri <- either throwIO return archiveResult
    hash <- getNixSha256ForUriUnpacked archiveUri
    return $ ThunkPtr
      { _thunkPtr_rev = ThunkRev
        { _thunkRev_commit = commitNameToRef commit
        , _thunkRev_nixSha256 = hash
        }
      , _thunkPtr_source = ThunkSource_GitHub $ GitHubSource
        { _gitHubSource_owner = owner
        , _gitHubSource_repo = repo
        , _gitHubSource_branch = branch
        , _gitHubSource_private = repoIsPrivate
        }
      }
  gitThunkPtr u _commit' _branch' = do
   case uriScheme u of
     "https:" -> return ()
     "git:" -> return ()
     _ -> fail "thunk pack: obelisk currently only supports https and git protocols for non-GitHub remotes"
   $notImplemented
  parseSshShorthand uri' = do
    -- This is what git does to check that the remote
    -- is not a local file path when parsing shorthand.
    -- Last referenced from here:
    -- https://github.com/git/git/blob/95ec6b1b3393eb6e26da40c565520a8db9796e9f/connect.c#L712
    let uri = T.pack uri'
        (authAndHostname, colonAndPath) = T.break (== ':') uri
        properUri = "ssh://" <> authAndHostname <> "/" <> T.drop 1 colonAndPath
    -- Shorthand is valid iff a colon is present and it occurs before the first slash
    -- This check is used to disambiguate a filepath containing a colon from shorthand
    guard $ isNothing (T.findIndex (=='/') authAndHostname)
         && not (T.null colonAndPath)
    parseURI $ T.unpack properUri
