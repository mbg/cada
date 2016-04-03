--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.TextBufferBenchmark where

--------------------------------------------------------------------------------

import Criterion.Main

import Data.Default.Class

import Cada.Compiler.Lexer.TextBuffer

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark = bgroup "text-buffer" [ ]

--------------------------------------------------------------------------------
