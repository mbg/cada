--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Project.Server (
    projectExistsCheck,
    projectServer
) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Except

import qualified Data.Map as M

import Data.Maybe
import Data.UUID
import Data.UUID.V4

import Servant.API
import Servant.Server

import System.Directory
import System.Log.Logger

import Cada.Compiler.Project.API
import Cada.Compiler.Project.File
import Cada.Compiler.State

--------------------------------------------------------------------------------

-- | `registerProject uuid` registers a new project with the specified UUID.
registerProject :: CompStVar -> UUID -> STM ()
registerProject st uuid = do
    cs@CompSt{..} <- readTVar st

    proj <- newTVar defaultProject

    writeTVar st $
        cs { compProjects = M.insert uuid proj compProjects }

-- | `lookupProject uuid` looks up the project whose globally unique ID
--   is `uuid`.
{-lookupProject :: UUID -> Query CompilerState (Maybe Project)
lookupProject uuid = do
    CompSt{..} <- ask
    return $ M.lookup uuid compProjects

-- | `withProject uuid`
withProject :: UUID -> (Project -> Project) -> Update CompilerState ()
withProject uuid f = do
    st <- get
    let compProjects' = M.adjust f uuid (compProjects st)
    put $ st { compProjects = compProjects' }
-}

projectExistsCheck :: UUID -> CompSrvM (TVar Project)
projectExistsCheck uuid = ask >>= \var -> do
    CompSt{..} <- liftIO $ readTVarIO var

    case M.lookup uuid compProjects of
        Nothing   -> throwE err404
        Just proj -> return proj

registerFile :: TVar Project -> FileInitReq -> CompSrvM FileInitRes
registerFile var FileInitReq{..} = do
    -- generate random UUIDs for this file
    uuid <- liftIO nextRandom
    ver  <- liftIO nextRandom

    case fileInitReqPath of
        Nothing -> liftIO $ do

            let
                file = newFileFromText ver (fromMaybe "" fileInitialContent)

            fileVar <- newTVarIO file

            -- register a new file
            atomically $ do
                p@Proj{..} <- readTVar var
                writeTVar var $
                    p { projFiles = M.insert uuid fileVar projFiles }

            infoM "Cada.Server" "Registered a new buffer!"
            infoM "Cada.Server" (show fileInitialContent)

            -- return information about this buffer
            return $ FileBufferInit $ VersionedFile {
                fileID = uuid,
                fileVersion = ver
            }
        Just path -> liftIO $ atomically $ do
            p@Proj{..} <- readTVar var

            case M.lookup path projFilePaths of
                Nothing -> undefined -- not part of the project
                Just uuid -> undefined -- already part of the project

--------------------------------------------------------------------------------



-- `initProject` handles a request for a new project.
initProject :: CompSrvM UUID
initProject = ask >>= \var -> liftIO $ do
    uuid <- nextRandom
    atomically (registerProject var uuid)
    return uuid

-- `initFile req` handles a request to initialise a new file.
initFile :: FileInitReq -> CompSrvM FileInitRes
initFile req = do
    -- check that the project exists
    proj <- projectExistsCheck (fileInitReqProject req)

    registerFile proj req

--------------------------------------------------------------------------------

-- | `projectServer` handles requests related to the projects API.
projectServer :: CompSrv ProjectAPI
projectServer = initProject
           :<|> initFile

--------------------------------------------------------------------------------
