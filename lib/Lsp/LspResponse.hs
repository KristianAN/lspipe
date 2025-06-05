{-# LANGUAGE OverloadedStrings #-}

module Lsp.LspResponse (extractCapabilities, LspResponse (..), LspResponseError (..)) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
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
    , id :: Maybe Int
    , result :: Value
    , error :: Maybe LspResponseError
    }
    deriving (Generic, Show)

instance ToJSON LspResponse
instance FromJSON LspResponse

extractKeyMap :: Value -> KM.KeyMap Value
extractKeyMap (Object km) = km
extractKeyMap _ = KM.empty

extractCapabilities :: LspResponse -> Maybe [T.Text]
extractCapabilities resp = do
    Object capabilities <- KM.lookup "capabilities" (extractKeyMap res)
    pure $ map (Key.toText) $ KM.keys capabilities
  where
    res = result resp
