--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Project.API (
    module Cada.Compiler.Project.Types,

    ProjectAPI
) where

--------------------------------------------------------------------------------

import Data.UUID

import Servant.API

import Cada.Compiler.Project.Types

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | The project API.
type ProjectAPI =
      "project" :>
      "init" :>
      Post '[JSON] UUID
 :<|> "project" :>
      "file" :>
      "init" :>
      ReqBody '[JSON] FileInitReq :>
      Post '[JSON] FileInitRes



--------------------------------------------------------------------------------
