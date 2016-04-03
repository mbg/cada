--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Client where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import Network.HTTP.Client

import Servant.API
import Servant.Client

import Cada.Compiler.Utility.UUID
import Cada.Compiler.API

--------------------------------------------------------------------------------

-- | The type of all client functions.
type ClientFun a = Manager -> BaseUrl -> ExceptT ServantError IO a

--------------------------------------------------------------------------------

-- | `initProject` initialises a new project and returns its UUID.
initProject :: ClientFun UUID

-- | `initFile req` requests the compiler to initialise a file.
initFile :: FileInitReq -> ClientFun FileInitRes

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

projectApi
    :<|> lexerApi = client compilerApi

initProject
    :<|> initFile = projectApi

--------------------------------------------------------------------------------
