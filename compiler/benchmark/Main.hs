--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

-- | Main module for the compiler benchmarks.
module Main (main) where

--------------------------------------------------------------------------------

import Criterion.Main

import qualified Cada.Compiler.Lexer.TextBufferBenchmark as TBB

--------------------------------------------------------------------------------

-- | `main` is the main entry point for this benchmark.
main :: IO ()
main = defaultMain [
    bgroup "lexer" [ TBB.benchmark
                   ]
    ]

--------------------------------------------------------------------------------
