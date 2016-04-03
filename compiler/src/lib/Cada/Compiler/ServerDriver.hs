--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.ServerDriver where

--------------------------------------------------------------------------------

import Control.Concurrent.STM

import Network.Wai.Handler.Warp

import System.IO (stdout)
import System.Log.Logger
import System.Log.Handler.Simple

import Cada.Compiler.Server
import Cada.Compiler.State

--------------------------------------------------------------------------------

-- | `runCompilerServer` runs an instance of the compiler as a server.
runCompilerServer :: IO ()
runCompilerServer = do
    -- initialise the compiler state
    st <- newTVarIO defaultCompSt

    h <- streamHandler stdout DEBUG
    updateGlobalLogger "Cada" (addHandler h >> setLevel DEBUG)

    infoM "Cada" "Starting server..."

    -- start the compiler server
    run 8081 (compilerApp st)

--------------------------------------------------------------------------------
