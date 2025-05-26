module Lsp.LspResponse (LspResponse (..), LspResponseError (..)) where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics (Generic)

data LspResponseError = LspResponseError
    { code :: Int
    , message :: T.Text
    }
    deriving (Generic, Show)

instance ToJSON LspResponseError
instance FromJSON LspResponseError

data LspResponse = LspResponse
    { jsonrpc :: T.Text
    , id :: Int
    , result :: Value
    , error :: Maybe LspResponseError
    }
    deriving (Generic, Show)

instance ToJSON LspResponse
instance FromJSON LspResponse
