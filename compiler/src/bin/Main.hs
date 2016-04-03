--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent

import Servant.JS

import Cada.Compiler.API
import Cada.Compiler.Internal.JS.ES6
import Cada.Compiler.ServerDriver

import System.Environment

--------------------------------------------------------------------------------

-- | `main` is the main entry point to the compiler process.
main :: IO ()
main = do
    xs <- getArgs
    print xs
    writeJSForAPI compilerApi (es6 defCommonGeneratorOptions { moduleName = "CadaApi" }) "api.js"
    threadId <- forkIO runCompilerServer

    getLine
    killThread threadId
    putStrLn "Hello world."



--------------------------------------------------------------------------------
