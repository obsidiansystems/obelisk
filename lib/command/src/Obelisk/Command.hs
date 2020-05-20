{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Command where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BSU
import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Traversable (for)
import Options.Applicative
import Options.Applicative.Help.Pretty (text, (<$$>))
import System.Directory
import System.Environment
import System.FilePath
import qualified System.Info
import System.IO (hIsTerminalDevice, stdout)
import Text.ShellEscape (bash)
import System.Posix.Process (executeFile)

import Obelisk.App
import Obelisk.CliApp
import Obelisk.Command.Deploy
import Obelisk.Command.Project
import Obelisk.Command.Run
import Obelisk.Command.Thunk
import qualified Obelisk.Command.VmBuilder as VmBuilder
import qualified Obelisk.Command.Preprocessor as Preprocessor


data Args = Args
  { _args_noHandOffPassed :: Bool
  -- ^ This flag is actually handled outside of the optparse-applicative parser, but we detect whether
  -- it has gotten through in order to notify the user that it should only be passed once and as the very
  -- first argument
  , _args_verbose :: Bool
  , _args_command :: ObCommand
  }
  deriving Show

newtype ArgsConfig = ArgsConfig
  { _argsConfig_enableVmBuilderByDefault :: Bool
  }

args :: ArgsConfig -> Parser Args
args cfg = Args <$> noHandoff <*> verbose <*> obCommand cfg

noHandoff :: Parser Bool
noHandoff = flag False True $ mconcat
  [ long "no-handoff"
  , help "Do not hand off execution to project-specific implementation of this command"
  , hidden
  ]

verbose :: Parser Bool
verbose = flag False True $ mconcat
  [ long "verbose"
  , short 'v'
  , help "Be more verbose"
  ]

argsInfo :: ArgsConfig -> ParserInfo Args
argsInfo cfg = info (args cfg <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Manage Obelisk projects"
  ]

initSource :: Parser InitSource
initSource = foldl1 (<|>)
  [ pure InitSource_Default
  , InitSource_Branch <$> strOption (long "branch" <> metavar "BRANCH" <> help "Initialize the project using the given BRANCH of Obelisk's official repository")
  , InitSource_Symlink <$> strOption (long "symlink" <> action "directory" <> metavar "PATH" <> help "(Use with caution) Initialize the project using the copy of Obelisk found at the given PATH")
  ]

initForce :: Parser Bool
initForce = switch (long "force" <> help "Allow ob init to overwrite files")

data ObCommand
   = ObCommand_Init InitSource Bool
   | ObCommand_Deploy DeployCommand
   | ObCommand_Run [(FilePath, Interpret)]
   | ObCommand_Profile String [String]
   | ObCommand_Thunk ThunkOption
   | ObCommand_Repl [(FilePath, Interpret)]
   | ObCommand_Watch [(FilePath, Interpret)]
   | ObCommand_Shell ShellOpts
   | ObCommand_Doc String [String] -- shell and list of packages
   | ObCommand_Hoogle String Int -- shell and port
   | ObCommand_Internal ObInternal
   deriving Show

data ObInternal
   -- the preprocessor argument syntax is also handled outside
   -- optparse-applicative, but it shouldn't ever conflict with another syntax
   = ObInternal_ApplyPackages String String String [String]
   | ObInternal_ExportGhciConfig
      [(FilePath, Interpret)]
      Bool -- ^ Use relative paths
   deriving Show


inNixShell' :: MonadObelisk m => StaticPtr (ObeliskT IO ()) -> m ()
inNixShell' p = withProjectRoot "." $ \root -> do
  cmd <- liftIO $ fmap (bash . BSU.fromString) <$> mkCmd
  projectProc root True "ghc" (Just cmd)
  where
    mkCmd = do
      argsCfg <- getArgsConfig
      myArgs <- getArgs
      obArgs <- parseCLIArgs argsCfg myArgs
      progName <- getObeliskExe
      return $ progName : catMaybes
        [ Just "--no-handoff"
        , bool Nothing (Just "--verbose") $ _args_verbose obArgs
        , Just "internal"
        , Just "run-static-io"
        , Just $ encodeStaticKey $ staticKey p
        ]

obCommand :: ArgsConfig -> Parser ObCommand
obCommand cfg = hsubparser
  (mconcat
    [ command "init" $ info (ObCommand_Init <$> initSource <*> initForce) $ progDesc "Initialize an Obelisk project"
    , command "deploy" $ info (ObCommand_Deploy <$> deployCommand cfg) $ progDesc "Prepare a deployment for an Obelisk project"
    , command "run" $ info (ObCommand_Run <$> interpretOpts) $ progDesc "Run current project in development mode"
    , command "profile" $ info (uncurry ObCommand_Profile <$> profileCommand) $ progDesc "Run current project with profiling enabled"
    , command "thunk" $ info (ObCommand_Thunk <$> thunkOption) $ progDesc "Manipulate thunk directories"
    , command "repl" $ info (ObCommand_Repl <$> interpretOpts) $ progDesc "Open an interactive interpreter"
    , command "watch" $ info (ObCommand_Watch <$> interpretOpts) $ progDesc "Watch current project for errors and warnings"
    , command "shell" $ info (ObCommand_Shell <$> shellOpts) $ progDesc "Enter a shell with project dependencies or run a command in such a shell. E.g. ob shell -- ghc-pkg list"
    , command "doc" $ info (ObCommand_Doc <$> shellFlags <*> packageNames) $
        progDesc "List paths to haddock documentation for specified packages"
        <> footerDoc (Just $
              text "Hint: To open the documentation you can pipe the output of this command like"
              <$$> text "ob doc reflex reflex-dom-core | xargs -n1 xdg-open")
    , command "hoogle" $ info (ObCommand_Hoogle <$> shellFlags <*> portOpt 8080) $ progDesc "Run a hoogle server locally for your project's dependency tree"
    , command "internal" $ info (ObCommand_Internal <$> internalCommand) $ progDesc "Internal Obelisk commands with unstable APIs"
    ])

internalCommand :: Parser ObInternal
internalCommand = hsubparser $ mconcat
  [ command "export-ghci-configuration" $ info (ObInternal_ExportGhciConfig <$> interpretOpts <*> useRelativePathsFlag)
      $ progDesc "Export the GHCi configuration used by ob run, etc.; useful for IDE integration"
  ]
  where
    useRelativePathsFlag = switch (long "use-relative-paths" <> help "Use relative paths")

packageNames :: Parser [String]
packageNames = some (strArgument (metavar "PACKAGE-NAME..."))

deployCommand :: ArgsConfig -> Parser DeployCommand
deployCommand cfg = hsubparser $ mconcat
  [ command "init" $ info (DeployCommand_Init <$> deployInitOpts) $ progDesc "Initialize a deployment configuration directory"
  , command "push" $ info (DeployCommand_Push <$> remoteBuilderParser) mempty
  , command "test" $ info (DeployCommand_Test <$> platformP) $ progDesc "Test your obelisk project from a mobile platform."
  , command "update" $ info (pure DeployCommand_Update) $ progDesc "Update the deployment's src thunk to latest"
  ]
  where
    platformP = hsubparser $ mconcat
      [ command "android" $ info (pure (Android, [])) mempty
      , command "ios" $ info ((,) <$> pure IOS <*> fmap pure (strArgument (metavar "TEAMID" <> help "Your Team ID - found in the Apple developer portal"))) mempty
      ]

    remoteBuilderParser :: Parser (Maybe RemoteBuilder)
    remoteBuilderParser =
      flag (if enabledByDefault then enabled else Nothing) enabled (mconcat
        [ long $ "enable-" <> flagBase
        , help $ "Enable " <> flagDesc <> (if enabledByDefault then " (default)" else "")
        ])
      <|> flag enabled Nothing (mconcat
        [ long $ "disable-" <> flagBase
        , help $ "Disable a " <> flagDesc <> (if not enabledByDefault then " (default)" else "")
        ])
      where
        enabledByDefault = _argsConfig_enableVmBuilderByDefault cfg
        enabled = Just RemoteBuilder_ObeliskVM
        flagBase = "vm-builder"
        flagDesc = "managed Linux virtual machine as a Nix remote builder (requires Docker)"


deployInitOpts :: Parser DeployInitOpts
deployInitOpts = DeployInitOpts
  <$> strArgument (action "directory" <> metavar "DEPLOYDIR" <> help "Path to a directory where the deployment repository will be initialized")
  <*> strOption (long "ssh-key" <> action "file" <> metavar "SSHKEY" <> help "Path to an SSH key that will be *copied* to the deployment repository")
  <*> some (strOption (long "hostname" <> metavar "HOSTNAME" <> help "hostname of the deployment target"))
  <*> strOption (long "route" <> metavar "PUBLICROUTE" <> help "Publicly accessible URL of your app")
  <*> strOption (long "admin-email" <> metavar "ADMINEMAIL" <> help "Email address where administrative alerts will be sent")
  <*> flag True False (long "disable-https" <> help "Disable automatic https configuration for the backend")

type TeamID = String
data RemoteBuilder = RemoteBuilder_ObeliskVM
  deriving (Eq, Show)

data DeployCommand
  = DeployCommand_Init DeployInitOpts
  | DeployCommand_Push (Maybe RemoteBuilder)
  | DeployCommand_Test (PlatformDeployment, [String])
  | DeployCommand_Update
  deriving Show

profileCommand :: Parser (String, [String])
profileCommand = (,)
  <$> strOption
    (  long "output"
    <> short 'o'
    <> help "Base output to use for profiling output. Suffixes are added to this based on the profiling type. Defaults to a timestamped path in the profile/ directory in the project's root."
    <> metavar "PATH"
    <> value "profile/%Y-%m-%dT%H:%M:%S"
    <> showDefault
    )
  <*> (words <$> strOption
    (  long "rts-flags"
    <> help "RTS Flags to pass to the executable."
    <> value "-p -hc"
    <> metavar "FLAGS"
    <> showDefault
    ))

thunkConfig :: Parser ThunkConfig
thunkConfig = ThunkConfig
  <$>
    (   flag' (Just True) (long "private" <> help "Mark thunks as pointing to a private repository")
    <|> flag' (Just False) (long "public" <> help "Mark thunks as pointing to a public repository")
    <|> pure Nothing
    )

thunkUpdateConfig :: Parser ThunkUpdateConfig
thunkUpdateConfig = ThunkUpdateConfig
  <$> optional (strOption (long "branch" <> metavar "BRANCH" <> help "Use the given branch when looking for the latest revision"))
  <*> thunkConfig

thunkPackConfig :: Parser ThunkPackConfig
thunkPackConfig = ThunkPackConfig
  <$> switch (long "force" <> short 'f' <> help "Force packing thunks even if there are branches not pushed upstream, uncommitted changes, stashes. This will cause changes that have not been pushed upstream to be lost; use with care.")
  <*> thunkConfig

data ThunkOption = ThunkOption
  { _thunkOption_thunks :: NonEmpty FilePath
  , _thunkOption_command :: ThunkCommand
  } deriving Show

data ThunkCommand
  = ThunkCommand_Update ThunkUpdateConfig
  | ThunkCommand_Unpack
  | ThunkCommand_Pack ThunkPackConfig
  deriving Show

thunkOption :: Parser ThunkOption
thunkOption = hsubparser $ mconcat
  [ command "update" $ info (thunkOptionWith $ ThunkCommand_Update <$> thunkUpdateConfig) $ progDesc "Update packed thunk to latest revision available on the tracked branch"
  , command "unpack" $ info (thunkOptionWith $ pure ThunkCommand_Unpack) $ progDesc "Unpack thunk into git checkout of revision it points to"
  , command "pack" $ info (thunkOptionWith $ ThunkCommand_Pack <$> thunkPackConfig) $ progDesc "Pack git checkout or unpacked thunk into thunk that points at the current branch's upstream"
  ]
  where
    thunkOptionWith f = ThunkOption
      <$> ((NonEmpty.:|)
            <$> thunkDirArg (metavar "THUNKDIRS..." <> help "Paths to directories containing thunk data")
            <*> many (thunkDirArg mempty)
          )
      <*> f
    thunkDirArg opts = fmap (dropTrailingPathSeparator . normalise) $ strArgument $ action "directory" <> opts

data ShellOpts
  = ShellOpts
    { _shellOpts_shell :: String
    , _shellOpts_interpretPaths :: [(FilePath, Interpret)]
    , _shellOpts_command :: Maybe String
    }
  deriving Show

shellFlags :: Parser String
shellFlags =
  flag' "ghc" (long "ghc" <> help "Enter a shell environment having ghc (default)")
  <|> flag "ghc" "ghcjs" (long "ghcjs" <> help "Enter a shell having ghcjs rather than ghc")
  <|> strOption (short 'A' <> long "argument" <> metavar "NIXARG" <> help "Use the environment specified by the given nix argument of `shells'")

interpretOpts :: Parser [(FilePath, Interpret)]
interpretOpts = many
    (   (, Interpret_Interpret) <$>
          strOption (common <> long "interpret" <> help
            "Don't pre-build packages found in DIR when constructing the package database. The default behavior is \
            \'--interpret <project-root>', which will load everything which is unpacked into GHCi. \
            \ Use --interpret and --no-interpret multiple times to add or remove multiple trees \
            \ from the environment. Settings for right-most directories will \
            \ override settings for any identical directories given earlier."
          )
    <|> (, Interpret_NoInterpret) <$>
          strOption (common <> long "no-interpret" <> help
            "Make packages found in DIR available in the package database (but only when they are used dependencies). \
            \ This will build the packages in DIR before loading GHCi. \
            \See help for --interpret for how the two options are related."
          )
    )
  where
    common = action "directory" <> metavar "DIR"

shellOpts :: Parser ShellOpts
shellOpts = ShellOpts
  <$> shellFlags
  <*> interpretOpts
  -- This funny construction is used to support optparse-applicative's @--@ parsing.
  -- All arguments after @--@ are left unparsed and instead provided to the last positional parser
  -- which must therefore be 'many' in order to consume the rest of the input.
  <*> ((\xs -> if null xs then Nothing else Just $ unwords xs) <$> many (strArgument (metavar "COMMAND")))

portOpt :: Int -> Parser Int
portOpt dfault = option auto (long "port" <> short 'p' <> help "Port number for server" <> showDefault <> value dfault <> metavar "INT")

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnEmpty = True
  }

-- | Create an Obelisk config for the current process.
mkObeliskConfig :: IO Obelisk
mkObeliskConfig = do
  cliArgs <- getArgs
  -- This function should not use argument parser (full argument parsing happens post handoff)
  -- TODO: See if we can use the argument parser with a subset of the parsers to get logging level out.
  let logLevel = toLogLevel $ any (`elem` ["-v", "--verbose"]) cliArgs
  notInteractive <- not <$> isInteractiveTerm
  cliConf <- newCliConfig logLevel notInteractive notInteractive $ \case
    ObeliskError_ProcessError (ProcessFailure p code) ann ->
      ( "Process exited with code " <> T.pack (show code) <> "; " <> reconstructCommand p
        <> maybe "" ("\n" <>) ann
      , 2
      )
    ObeliskError_Unstructured msg -> (msg, 2)

  return $ Obelisk cliConf
  where
    toLogLevel = bool Notice Debug
    isInteractiveTerm = do
      isTerm <- hIsTerminalDevice stdout
      -- Running in bash/fish/zsh completion
      inShellCompletion <- liftIO $ isInfixOf "completion" . unwords <$> getArgs

      -- Respect the user’s TERM environment variable. Dumb terminals
      -- like Eshell cannot handle lots of control sequences that the
      -- spinner uses.
      termEnv <- lookupEnv "TERM"
      let isDumb = termEnv == Just "dumb"

      return $ isTerm && not inShellCompletion && not isDumb

-- | For use from development obelisk repls
--
-- Example:
-- > runCommand $ someFuncInMonadObelisk ...
runCommand :: ObeliskT IO a -> IO a
runCommand f = flip runObelisk f =<< mkObeliskConfig

main :: IO ()
main = runCommand . main' =<< getArgsConfig

main' :: MonadObelisk m => ArgsConfig -> m ()
main' argsCfg = do
  obPath <- liftIO getExecutablePath
  myArgs <- liftIO getArgs
  logLevel <- getLogLevel
  putLog Debug $ T.pack $ unwords
    [ "Starting Obelisk <" <> obPath <> ">"
    , "args=" <> show myArgs
    , "logging-level=" <> show logLevel
    ]

  --TODO: We'd like to actually use the parser to determine whether to hand off,
  --but in the case where this implementation of 'ob' doesn't support all
  --arguments being passed along, this could fail.  For now, we don't bother
  --with optparse-applicative until we've done the handoff.
  let go as = do
        args' <- liftIO $ handleParseResult (execParserPure parserPrefs (argsInfo argsCfg) as)
        case _args_noHandOffPassed args' of
          False -> return ()
          True -> putLog Warning "--no-handoff should only be passed once and as the first argument; ignoring"
        ob $ _args_command args'
      handoffAndGo as = findProjectObeliskCommand "." >>= \case
        Nothing -> go as -- If not in a project, just run ourselves
        Just impl -> do
          -- Invoke the real implementation, using --no-handoff to prevent infinite recursion
          putLog Debug $ "Handing off to " <> T.pack impl
          _ <- liftIO $ rawSystem impl ("--no-handoff" : myArgs)
          return ()
  case myArgs of
    "--no-handoff" : as -> go as -- If we've been told not to hand off, don't hand off
    origPath:inPath:outPath:preprocessorName:packagePaths
      | preprocessorName == preprocessorIdentifier && any (\c -> c == '.' || c == pathSeparator) origPath ->
        ob $ ObCommand_Internal $ ObInternal_ApplyPackages origPath inPath outPath packagePaths
    a:as -- Otherwise bash completion would always hand-off even if the user isn't trying to
      | "--bash-completion" `isPrefixOf` a
      && "--no-handoff" `elem` as -> go (a:as)
      | otherwise -> handoffAndGo (a:as)
    as -> handoffAndGo as

ob :: MonadObelisk m => ObCommand -> m ()
ob = \case
  ObCommand_Init source force -> initProject source force
  ObCommand_Deploy dc -> case dc of
    DeployCommand_Init deployOpts -> withProjectRoot "." $ \root -> deployInit deployOpts root
    DeployCommand_Push remoteBuilder -> do
      deployPath <- liftIO $ canonicalizePath "."
      deployPush deployPath $ case remoteBuilder of
        Nothing -> pure []
        Just RemoteBuilder_ObeliskVM -> (:[]) <$> VmBuilder.getNixBuildersArg
    DeployCommand_Update -> deployUpdate "."
    DeployCommand_Test (platform, extraArgs) -> deployMobile platform extraArgs
  ObCommand_Run interpretPathsList -> withInterpretPaths interpretPathsList run
  ObCommand_Profile basePath rtsFlags -> profile basePath rtsFlags
  ObCommand_Thunk to -> case _thunkOption_command to of
    ThunkCommand_Update config -> for_ thunks (updateThunkToLatest config)
    ThunkCommand_Unpack -> for_ thunks unpackThunk
    ThunkCommand_Pack config -> for_ thunks (packThunk config)
    where
      thunks = _thunkOption_thunks to
  ObCommand_Repl interpretPathsList -> withInterpretPaths interpretPathsList runRepl
  ObCommand_Watch interpretPathsList -> withInterpretPaths interpretPathsList runWatch
  ObCommand_Shell (ShellOpts shellAttr interpretPathsList cmd) -> withInterpretPaths interpretPathsList $ \root interpretPaths ->
    nixShellForInterpretPaths False shellAttr root interpretPaths cmd -- N.B. We do NOT bash escape here; we want to run the command as-is
  ObCommand_Doc shellAttr pkgs -> withInterpretPaths [] $ \root interpretPaths ->
    nixShellForInterpretPaths True shellAttr root interpretPaths $ Just $ haddockCommand pkgs
  ObCommand_Hoogle shell' port -> withProjectRoot "." $ \root -> do
    nixShellWithHoogle root True shell' $ Just $ "hoogle server -p " <> show port <> " --local"
  ObCommand_Internal icmd -> case icmd of
    ObInternal_ApplyPackages origPath inPath outPath packagePaths -> do
      liftIO $ Preprocessor.applyPackages origPath inPath outPath packagePaths
    ObInternal_ExportGhciConfig interpretPathsList useRelativePaths ->
      liftIO . putStrLn . unlines =<< withInterpretPaths interpretPathsList (exportGhciConfig useRelativePaths)

-- | A helper for the common case that the command you want to run needs the project root and a resolved
-- set of interpret paths.
withInterpretPaths :: MonadObelisk m => [(FilePath, Interpret)] -> (FilePath -> PathTree Interpret -> m a) -> m a
withInterpretPaths interpretPathsList f = withProjectRoot "." $ \root -> do
  interpretPaths' <- resolveInterpretPaths $ (root, Interpret_Interpret) : interpretPathsList
  case interpretPaths' of
    Nothing -> failWith "No paths provided for finding packages"
    Just interpretPaths -> f root interpretPaths

haddockCommand :: [String] -> String
haddockCommand pkgs = unwords
  [ "for p in"
  , unwords [getHaddockPath p ++ "/index.html" | p <- pkgs]
  , "; do echo $p; done"
  ]
  where getHaddockPath p = "$(ghc-pkg field " ++ p ++ " haddock-html --simple-output)"

--TODO: Clean up all the magic strings throughout this codebase

getArgsConfig :: IO ArgsConfig
getArgsConfig = pure $ ArgsConfig { _argsConfig_enableVmBuilderByDefault = System.Info.os == "darwin" }

-- | Resolves an ordered list of paths for use with @--interpret@/@--no-interpret@ by coalescing
--   paths into a non-ambiguous set of paths. Ambiguity is resolved by choosing right-most paths
--   over any preceeding identical paths.
--
--   For example: @a/b=ON a/b/c=OFF@ and @a/b/c=OFF a/b=ON@ are the same.
--   @a/b=ON a/b=OFF@ is reduced to @a/b=OFF@. We prefer right-biased choice to increase
--   scriptability.
--
--   N.B. All the paths in the result will be canonicalized. It's impossible to determine path
--   overlap otherwise.
resolveInterpretPaths :: MonadIO m => [(FilePath, a)] -> m (Maybe (PathTree a))
resolveInterpretPaths ps = do
  trees <- liftIO $ for ps $ \(p, a) -> pathToTree a <$> canonicalizePath p
  pure $ foldr1 mergeTrees <$> nonEmpty trees
  where
    -- | Merge two 'PathTree's preferring leaves on the right in as much as they overlap with paths on the left.
    mergeTrees :: PathTree a -> PathTree a -> PathTree a
    mergeTrees (PathTree_Node ax x) (PathTree_Node ay y) = PathTree_Node (ay <|> ax) $ Map.unionWith mergeTrees x y
