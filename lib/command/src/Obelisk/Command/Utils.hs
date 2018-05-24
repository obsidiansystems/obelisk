{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Obelisk.Command.Utils where

import Control.Applicative hiding (many)
import Control.Monad.Except
import Data.Bool (bool)
import qualified Text.Megaparsec.Char.Lexer as ML
import Data.Bifunctor
import qualified Data.List as L
import Data.Char
import Data.Either
import Data.Semigroup ((<>))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import System.Directory (canonicalizePath)
import System.Environment (getExecutablePath)
import qualified System.Process as P
import Text.Megaparsec as MP
import Text.Megaparsec.Char as MP

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

getObeliskExe :: IO FilePath
getObeliskExe = getExecutablePath >>= canonicalizePath

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

gitProc :: FilePath -> [String] -> P.CreateProcess
gitProc repo argsRaw =
  P.proc "git" $ runGitInDir argsRaw
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

-- | Recursively copy a directory using `cp -a`
copyDir :: FilePath -> FilePath -> P.CreateProcess
copyDir src dest =
  (P.proc "cp" ["-a", ".", dest]) { P.cwd = Just src }

readGitProcess :: MonadObelisk m => FilePath -> [String] -> m Text
readGitProcess repo = readProcessAndLogStderr Notice . gitProc repo

processToShellString :: FilePath -> [String] -> String
processToShellString cmd args = unwords $ map quoteAndEscape (cmd : args)
  where quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

-- | A simpler wrapper for CliApp's most used process function with sensible defaults.
runProc :: MonadObelisk m => P.CreateProcess -> m ()
runProc = callProcessAndLogOutput (Notice, Error)

-- | Like runProc, but all output goes to Debug logging level
runProcSilently :: MonadObelisk m => P.CreateProcess -> m ()
runProcSilently = callProcessAndLogOutput (Debug, Debug)

-- | A simpler wrapper for CliApp's readProcessAndLogStderr with sensible defaults.
readProc :: MonadObelisk m => P.CreateProcess -> m Text
readProc = readProcessAndLogStderr Error

tshow :: Show a => a -> Text
tshow = T.pack . show

-- like gitLsRemote, but also returns the ref branch if present
-- NOTE: getting a branch for a symref like HEAD relies on a minimum of git 1.8.5
gitLsRemoteHEAD :: String -> IO (Either String (CommitId, Maybe GitRef))
gitLsRemoteHEAD repository = runExceptT $ do
  let symref = GitRef_Head
  xs <- ExceptT $ gitLsRemote repository symref
  let (commits, refs) = bimap M.fromList M.fromList $ partitionEithers $ flip fmap xs $ \case
        (r, Left commitId) -> Left (r, commitId)
        (r, Right ref) -> Right (r, ref)
  case M.lookup symref commits of
    Nothing -> fail $ "gitLsRemoteHead: Did not find " <> T.unpack (showGitRef symref) <> " in the refs of " <> repository
    Just commitId -> return (commitId, M.lookup symref refs)

gitLsRemoteRef :: String -> GitRef -> IO (Either String (CommitId, Maybe GitRef))
gitLsRemoteRef repository ref = runExceptT $ do
  xs <- ExceptT $ gitLsRemote repository ref
  case ref of
    GitRef_Head -> do
      let (commits, refs) = bimap M.fromList M.fromList $ partitionEithers $ flip fmap xs $ \case
            (r, Left commitId) -> Left (r, commitId)
            (r, Right symref) -> Right (r, symref)
      case M.lookup ref commits of
        Nothing -> fail $ "gitLsRemoteHead: Did not find " <> T.unpack (showGitRef ref) <> " in the refs of " <> repository
        Just commitId -> return (commitId, M.lookup ref refs)
    _ -> case L.lookup ref xs of
      Just (Left a) -> return (a, Nothing)
      _ -> fail $ "gitLsRemoteHead: Did not find " <> T.unpack (showGitRef ref) <> " in the refs of " <> repository

gitLsRemote :: String -> GitRef -> IO (Either String [(GitRef, LsRemoteResult)])
gitLsRemote repository ref = do
  res <- P.readProcess "git" ["ls-remote", "--symref", repository, T.unpack $ showGitRef ref] mempty
  let t = T.pack res
  return $ first (MP.parseErrorPretty' t) $ MP.runParser (many parseLsRemote) "" t

lexeme :: Parsec Void Text a -> Parsec Void Text a
lexeme = ML.lexeme space

-- $ git ls-remote --symref git@github.com:obsidiansystems/obelisk.git HEAD
-- ref: refs/heads/master	HEAD
-- d0a8d25dc93f0acd096bc4ff2f550da9e2d0c8f5	refs/heads/master
parseLsRemote :: Parsec Void Text (GitRef, LsRemoteResult)
parseLsRemote = try parseRef <|> parseCommit
  where
    parseRef, parseCommit :: Parsec Void Text (GitRef, LsRemoteResult)
    parseRef = do
      _ <- lexeme "ref:"
      ref <- lexeme $ MP.takeWhileP (Just "ref")
        $ not . isSpace
      symbolicRef <- lexeme $ MP.takeWhileP (Just "symbolic ref")
        $ not . isSpace
      _ <- MP.eol
      return (toGitRef symbolicRef, Right $ toGitRef ref)
    parseCommit = do
      commitId <- lexeme $ MP.takeWhileP (Just "commit id")
        $ not . isSpace
      ref <- lexeme $ MP.takeWhileP (Just "ref")
        $ not . isSpace
      _ <- MP.eol
      return (toGitRef ref, Left commitId)

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
toGitRef r =
  if | "HEAD" == r -> GitRef_Head
     | Just s <- "refs/heads/" `T.stripPrefix` r -> GitRef_Branch s
     | Just s <- "refs/tags/" `T.stripPrefix` r -> GitRef_Tag s
     | otherwise -> GitRef_Other r

type CommitId = Text
type LsRemoteResult = Either CommitId GitRef
