{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
module Obelisk.Command.Utils where

import Control.Applicative hiding (many)
import Control.Monad.Except
import Data.Bool (bool)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Exit (ExitCode)
import System.Which (staticWhich)
import qualified Text.Megaparsec.Char.Lexer as ML
import Text.Megaparsec as MP
import Text.Megaparsec.Char as MP

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

cp :: FilePath
cp = $(staticWhich "cp")

mvPath :: FilePath
mvPath = $(staticWhich "mv")

rmPath :: FilePath
rmPath = $(staticWhich "rm")

ghcidExePath :: FilePath
ghcidExePath = $(staticWhich "ghcid")

findExePath :: FilePath
findExePath = $(staticWhich "find")

nixExePath :: FilePath
nixExePath = $(staticWhich "nix")

nixBuildExePath :: FilePath
nixBuildExePath = $(staticWhich "nix-build")

jreKeyToolPath :: FilePath
jreKeyToolPath = $(staticWhich "keytool")

nixPrefetchGitPath :: FilePath
nixPrefetchGitPath = $(staticWhich "nix-prefetch-git")

nixPrefetchUrlPath :: FilePath
nixPrefetchUrlPath = $(staticWhich "nix-prefetch-url")

nixShellPath :: FilePath
nixShellPath = $(staticWhich "nix-shell")

rsyncPath :: FilePath
rsyncPath = $(staticWhich "rsync")

sshPath :: FilePath
sshPath = $(staticWhich "ssh")

gitPath :: FilePath
gitPath = $(staticWhich "git")

whichPath :: FilePath
whichPath = $(staticWhich "which")

lnPath :: FilePath
lnPath = $(staticWhich "ln")

sshKeygenPath :: FilePath
sshKeygenPath = $(staticWhich "ssh-keygen")

-- $(staticWhich "docker") was intentionally omitted, at least for now
-- One concern is that I don't know how particular docker is about having the
-- CLI exe match the version of the docker daemon, which is largely outside of
-- the control of obelisk-command.
-- TODO: Investigate the tradeoffs associated with this choice
dockerPath :: FilePath
dockerPath = "docker"

-- | Nix syntax requires relative paths to be prefixed by @./@ or
-- @../@. This will make a 'FilePath' that can be embedded in a Nix
-- expression.
toNixPath :: FilePath -> FilePath
toNixPath root | "/" `isInfixOf` root = root
               | otherwise = "./" <> root


-- Check whether the working directory is clean
checkGitCleanStatus :: MonadObelisk m => FilePath -> Bool -> m Bool
checkGitCleanStatus repo withIgnored = do
  let
    runGit = readProcessAndLogStderr Debug . gitProc repo
    gitStatus = runGit $ ["status", "--porcelain"] <> bool [] ["--ignored"] withIgnored
    gitDiff = runGit ["diff"]
  T.null <$> liftA2 (<>) gitStatus gitDiff

-- | Ensure that git repo is clean
ensureCleanGitRepo :: MonadObelisk m => FilePath -> Bool -> Text -> m ()
ensureCleanGitRepo path withIgnored s =
  withSpinnerNoTrail ("Ensuring clean git repo at " <> T.pack path) $ do
    checkGitCleanStatus path withIgnored >>= \case
      False -> do
        statusDebug <- readGitProcess path $ ["status"] <> bool [] ["--ignored"] withIgnored
        putLog Warning "Working copy is unsaved; git status:"
        putLog Notice statusDebug
        failWith s
      True -> pure ()

initGit :: MonadObelisk m => FilePath -> m ()
initGit repo = do
  let git = callProcessAndLogOutput (Debug, Debug) . gitProc repo
  git ["init"]
  git ["add", "."]
  git ["commit", "-m", "Initial commit."]

gitProcNoRepo :: [String] -> ProcessSpec
gitProcNoRepo args = setEnvOverride (M.singleton "GIT_TERMINAL_PROMPT" "0" <>) $ proc gitPath args

gitProc :: FilePath -> [String] -> ProcessSpec
gitProc repo = gitProcNoRepo . runGitInDir
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

isolateGitProc :: ProcessSpec -> ProcessSpec
isolateGitProc = setEnvOverride (overrides <>)
  where
    overrides = M.fromList
      [ ("HOME", "/dev/null")
      , ("GIT_CONFIG_NOSYSTEM", "1")
      , ("GIT_TERMINAL_PROMPT", "0") -- git 2.3+
      , ("GIT_ASKPASS", "echo") -- pre git 2.3 to just use empty password
      , ("GIT_SSH_COMMAND", "ssh -o PreferredAuthentications=password -o PubkeyAuthentication=no -o GSSAPIAuthentication=no")
      ]

-- | Recursively copy a directory using `cp -a` -- TODO: Should use -rT instead of -a
copyDir :: FilePath -> FilePath -> ProcessSpec
copyDir src dest =
  setCwd (Just src) $ proc cp ["-a", ".", dest] -- TODO: This will break if dest is relative since we change cwd

readGitProcess :: MonadObelisk m => FilePath -> [String] -> m Text
readGitProcess repo = readProcessAndLogOutput (Debug, Notice) . gitProc repo

readGitProcessNoRepo :: MonadObelisk m => [String] -> m Text
readGitProcessNoRepo = readProcessAndLogOutput (Debug, Notice) . gitProcNoRepo

processToShellString :: FilePath -> [String] -> String
processToShellString cmd args = unwords $ map quoteAndEscape (cmd : args)
  where quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

-- | A simpler wrapper for CliApp's most used process function with sensible defaults.
runProc :: MonadObelisk m => ProcessSpec -> m ()
runProc = callProcessAndLogOutput (Notice, Error)

-- | Like runProc, but all output goes to Debug logging level
runProcSilently :: MonadObelisk m => ProcessSpec -> m ()
runProcSilently = callProcessAndLogOutput (Debug, Debug)

-- | A simpler wrapper for CliApp's readProcessAndLogStderr with sensible defaults.
readProc :: MonadObelisk m => ProcessSpec -> m Text
readProc = readProcessAndLogOutput (Debug, Error)

tshow :: Show a => a -> Text
tshow = T.pack . show

gitLookupDefaultBranch :: GitLsRemoteMaps -> Either Text Text
gitLookupDefaultBranch (refs, _) = do
  ref <- case M.lookup GitRef_Head refs of
    Just ref -> pure ref
    Nothing -> throwError
      "No symref entry for HEAD. \
      \ Is your git version at least 1.8.5? \
      \ Otherwise `git ls-remote --symref` will not work."
  case ref of
    GitRef_Branch b -> pure b
    _ -> throwError $
      "Default ref " <> showGitRef ref <> " is not a branch!"

gitLookupCommitForRef :: GitLsRemoteMaps -> GitRef -> Either Text CommitId
gitLookupCommitForRef (_, commits) ref = case M.lookup ref commits of
  Just a -> pure a
  Nothing -> throwError $ "Did not find commit for " <> showGitRef ref

gitLsRemote
  :: MonadObelisk m
  => String
  -> Maybe GitRef
  -> Maybe String
  -> m (ExitCode, GitLsRemoteMaps)
gitLsRemote repository mRef mBranch = do
  (exitCode, out, _err) <- case mBranch of
    Nothing -> readCreateProcessWithExitCode $ gitProcNoRepo $
        ["ls-remote", "--exit-code", "--symref", repository]
        ++ maybeToList (T.unpack . showGitRef <$> mRef)
    Just branchName -> readCreateProcessWithExitCode $ gitProcNoRepo
        ["ls-remote", "--exit-code", repository, branchName]
  let t = T.pack out
  maps <- case MP.runParser parseLsRemote "" t of
    Left err -> failWith $ T.pack $ MP.errorBundlePretty err
    Right table -> pure $ bimap M.fromList M.fromList $ partitionEithers table
  putLog Debug $ "git ls-remote maps: " <> T.pack (show maps)
  pure (exitCode, maps)

lexeme :: Parsec Void Text a -> Parsec Void Text a
lexeme = ML.lexeme $ void $ MP.takeWhileP (Just "within-line white space") $
  flip elem [' ', '\t']

-- $ git ls-remote --symref git@github.com:obsidiansystems/obelisk.git HEAD
-- ref: refs/heads/master	HEAD
-- d0a8d25dc93f0acd096bc4ff2f550da9e2d0c8f5	refs/heads/master
parseLsRemote :: Parsec Void Text [Either (GitRef, GitRef) (GitRef, CommitId)]
parseLsRemote =
  many ((fmap Left (try parseRef) <|> fmap Right parseCommit) <* try MP.eol) <* MP.eof
  where
    parseRef :: Parsec Void Text (GitRef, GitRef)
    parseRef = MP.label "ref and symbolic ref" $ do
      _ <- lexeme "ref:"
      ref <- lexeme $ MP.takeWhileP (Just "ref") $ not . isSpace
      symbolicRef <- lexeme $ MP.takeWhileP (Just "symbolic ref") $ not . isSpace
      return (toGitRef symbolicRef, toGitRef ref)
    parseCommit :: Parsec Void Text (GitRef, CommitId)
    parseCommit = MP.label "commit and ref" $ do
      commitId <- lexeme $ MP.takeWhileP (Just "commit id") $ not . isSpace
      ref <- lexeme $ MP.takeWhileP (Just "ref") $ not . isSpace
      return (toGitRef ref, commitId)

data GitRef
  = GitRef_Head
  | GitRef_Branch Text
  | GitRef_Tag Text
  | GitRef_Other Text
  deriving (Show, Eq, Ord)

showGitRef :: GitRef -> Text
showGitRef = \case
  GitRef_Head -> "HEAD"
  GitRef_Branch x -> "refs/heads/" <> x
  GitRef_Tag x -> "refs/tags/" <> x
  GitRef_Other x -> x

toGitRef :: Text -> GitRef
toGitRef = \case
  "HEAD" -> GitRef_Head
  r -> if
    | Just s <- "refs/heads/" `T.stripPrefix` r -> GitRef_Branch s
    | Just s <- "refs/tags/" `T.stripPrefix` r -> GitRef_Tag s
    | otherwise -> GitRef_Other r

type CommitId = Text

type GitLsRemoteMaps = (Map GitRef GitRef, Map GitRef CommitId)
