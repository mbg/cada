--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.TokenBuffer where

--------------------------------------------------------------------------------

import Data.FingerTree
import Data.Monoid

--------------------------------------------------------------------------------

-- |
--newtype TokenBuffer t = TokenBuffer (FingerTree Size t)

data Chunk t
    = Parsed [t]
    | Dirty Int

data Size = Size {
    chars     :: !Int,
    lookAhead :: !Int,
    dirty     :: !Bool
} deriving (Eq, Show)

instance Monoid Size where
    mempty = Size 0 0 False

    Size c1 l1 d1 `mappend` Size c2 l2 d2 =
        Size (c1+c2) (max l2 (l1 - c2)) (d1 || d2)



--------------------------------------------------------------------------------
