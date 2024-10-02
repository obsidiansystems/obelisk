{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}

module Obelisk.App where

import Control.Lens
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, ReaderT (..), ask, runReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Text (Text)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import Control.Monad.Log (MonadLog)
import Cli.Extras.Types
import "nix-thunk" Nix.Thunk (NixThunkError)

#if !MIN_VERSION_base(4,18,0)
import Control.Monad.Fail (MonadFail)
#endif

import Cli.Extras
  ( ProcessFailure
  , AsProcessFailure (..)
  , AsUnstructuredError (..)
  , runCli
  )

-- | An error thrown by one of the child processes invoked during
-- execution of Obelisk.
data ObeliskProcessError
  = ObeliskProcessError
    { _obeliskProcessError_failure :: ProcessFailure
      -- ^ The 'ProcessFailure' indicating both how the process was
      -- created and its eventual exit code.
    , _obeliskProcessError_mComment :: Maybe Text
      -- ^ Optionally, a comment can be attached to the 'ProcessFailure'
      -- to give the user more details.
    }

-- | An error thrown by the Obelisk app.
data ObeliskError
  = ObeliskError_ProcessError ObeliskProcessError
    -- ^ Indicates an error in one of our child processes.
  | ObeliskError_NixThunkError NixThunkError
    -- ^ Propagated errors from @nix-thunk@
  | ObeliskError_Unstructured Text
    -- ^ An ad-hoc error.

makePrisms ''ObeliskError

instance AsUnstructuredError ObeliskError where
  asUnstructuredError = _ObeliskError_Unstructured

-- Only project when the other field is null, otherwise we are not law abiding.
instance AsProcessFailure ObeliskError where
  asProcessFailure = _ObeliskError_ProcessError . prism (flip ObeliskProcessError Nothing) f
    where f = \case
            ObeliskProcessError pf Nothing -> Right pf
            pann@(ObeliskProcessError _ (Just _)) -> Left pann

newtype Obelisk = Obelisk
  { _obelisk_cliConfig :: CliConfig ObeliskError
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (CliT ObeliskError m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFail
    , MonadLog Output -- CliLog
    , MonadError ObeliskError -- CliThrow ObeliskError
    , HasCliConfig ObeliskError
    )

instance MonadTrans ObeliskT where
  lift = ObeliskT . lift . lift

class Monad m => HasObelisk m where
  getObelisk :: m Obelisk

instance Monad m => HasObelisk (ObeliskT m) where
  getObelisk = ObeliskT ask

instance HasObelisk m => HasObelisk (ReaderT r m) where
  getObelisk = lift getObelisk

instance (Monoid w, HasObelisk m) => HasObelisk (WriterT w m) where
  getObelisk = lift getObelisk

instance HasObelisk m => HasObelisk (StateT r m) where
  getObelisk = lift getObelisk

instance HasObelisk m => HasObelisk (ExceptT e m) where
  getObelisk = lift getObelisk

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    runCli (_obelisk_cliConfig c)
  . flip runReaderT c
  . unObeliskT

-- | Wrap an action which may throw 'NixThunkError' (e.g.
-- 'nixBuildAttrWithCache') in a 'MonadError' which supports throwing
-- 'ObeliskError'.
wrapNixThunkError
  :: (MonadError ObeliskError m, HasCliConfig ObeliskError m, MonadIO m)
  => CliT NixThunkError m a
  -> m a
wrapNixThunkError k = do
  cfg <- getCliConfig
  let cfg' = cfg { _cliConfig_errorLogExitCode = _cliConfig_errorLogExitCode cfg . ObeliskError_NixThunkError }
  runCli cfg' k

type MonadInfallibleObelisk m =
  ( CliLog m
  , HasCliConfig ObeliskError m
  , HasObelisk m
  , MonadIO m
  , MonadMask m
  )

type MonadObelisk m =
  ( MonadInfallibleObelisk m
  , CliThrow ObeliskError m
  , MonadFail m
  )

getObeliskUserStateDir :: IO FilePath
getObeliskUserStateDir = getXdgDirectory XdgData "obelisk"
