module Lsp.LspRequest (LspRequest (..)) where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics (Generic)

data LspRequest = LspRequest
    { jsonrpc :: T.Text
    , id :: Maybe Int
    , method :: T.Text
    , params :: Value
    }
    deriving (Generic, Show)

instance ToJSON LspRequest
instance FromJSON LspRequest

data LspResponseError = LspResponseError
    { code :: Int
    , message :: T.Text
    }
    deriving (Generic, Show)

instance ToJSON LspResponseError
instance FromJSON LspResponseError
