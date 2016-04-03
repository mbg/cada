--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.State where

--------------------------------------------------------------------------------

import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Data.Map                           as M
import Data.Typeable

import Servant.Server

import Cada.Compiler.Utility.UUID
import Cada.Compiler.Project.Types

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | The state of the compiler...
data CompilerState = CompSt {
    compProjects :: M.Map UUID (TVar Project) -- ^ Active projects
} deriving (Typeable)

defaultCompSt :: CompilerState
defaultCompSt = CompSt {
    compProjects = M.empty
}

--------------------------------------------------------------------------------

type CompStVar = TVar CompilerState

--------------------------------------------------------------------------------

type CompSrvM = ExceptT ServantErr (ReaderT CompStVar IO)

type CompSrv api = ServerT api CompSrvM

stateToHandler :: CompStVar -> CompSrvM :~> ExceptT ServantErr IO
stateToHandler st = Nat $ \m -> do
    r <- liftIO (runReaderT (runExceptT m) st)
    case r of
        Left err -> throwE err
        Right v -> return v

{-class MonadState (AcidState s) m => AcidMonad s m where
    update :: (UpdateEvent event, EventState event ~ s) => event -> m (EventResult event)
    query  :: (QueryEvent event, EventState event ~ s) => event -> m (EventResult event)

instance MonadIO m => AcidMonad st (StateT (AcidState st) m) where
     update event = do
         st <- get
         liftIO $ Acid.update st event

     query event = do
         st <- get
         liftIO $ Acid.query st event

instance AcidMonad s m => AcidMonad s (ExceptT err m) where
    update event = lift (update event)
    query event = lift (query event)-}

--------------------------------------------------------------------------------
