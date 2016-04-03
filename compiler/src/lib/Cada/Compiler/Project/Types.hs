--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Project.Types where

--------------------------------------------------------------------------------

import GHC.Generics

import Control.Concurrent.STM
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text

import Cada.Compiler.Utility.UUID
import Cada.Compiler.Project.File

--------------------------------------------------------------------------------

data Project = Proj {
    projFiles     :: M.Map UUID (TVar ProjFile),
    projFilePaths :: M.Map FilePath UUID
}

-- | `defaultProject` is an empty project.
defaultProject :: Project
defaultProject = Proj {
    projFiles = M.empty,
    projFilePaths = M.empty
}

--------------------------------------------------------------------------------

-- | Represents file metadata.
data VersionedFile = VersionedFile {
    fileID      :: UUID,
    fileVersion :: UUID
}

instance ToJSON VersionedFile where
    toJSON VersionedFile{..} =
        object [ "id"      .= fileID
               , "version" .= fileVersion
               ]

instance FromJSON VersionedFile where
    parseJSON (Object v) =
        VersionedFile <$> v .: "id"
                      <*> v .: "version"

--------------------------------------------------------------------------------

-- | Represents a request to initialise a file.
data FileInitReq = FileInitReq {
    fileInitReqProject :: UUID,
    -- | The path to the file, if any. If no path is specified, then the file
    --   will be treated as a buffer.
    fileInitReqPath    :: Maybe FilePath,
    -- | The initial content of the file, if any. Only applicable if the file
    --   is a buffer.
    fileInitialContent :: Maybe Text
}

instance ToJSON FileInitReq where
    toJSON FileInitReq{..} =
        object [ "project" .= fileInitReqProject
               , "path"    .= fileInitReqPath
               , "content" .= fileInitialContent
               ]

instance FromJSON FileInitReq where
    parseJSON (Object v) =
        FileInitReq <$> v .:  "project"
                    <*> v .:? "path"
                    <*> v .:? "content"

data FileInitRes
    = FileBufferInit {
        fileBufferInit :: VersionedFile
    }
    deriving (Generic)

instance ToJSON FileInitRes where
    toJSON FileBufferInit{..} =
        object [ "buffer"      .= toJSON fileBufferInit ]

instance FromJSON FileInitRes where
    parseJSON = genericParseJSON defaultOptions {
        constructorTagModifier = \xs -> case xs of
            "FileBufferInit" -> "buffer"
    }

--------------------------------------------------------------------------------
