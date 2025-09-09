{-# LANGUAGE OverloadedStrings #-}

module Lsp.Merger (mergeLspInitMessages, mergeLspResponses) where

import Data.Aeson
import Data.Aeson.Extra.Merge
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldl')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Lsp.LspResponse qualified as LSPR
import Prelude hiding (id)

maybeObject :: Value -> Maybe Object
maybeObject (Object obj) = Just obj
maybeObject _ = Nothing

data LspInitResponse = LspInitResponse
    { id :: Int
    , jsonrpc :: T.Text
    , result :: LspResult
    }
    deriving (Show, Generic)

data LspResult = LspResult
    { capabilities :: Value
    , serverInfo :: ServerInfo
    }
    deriving (Show, Generic)

data ServerInfo = ServerInfo
    { name :: T.Text
    , version :: T.Text
    }
    deriving (Show, Generic)

instance FromJSON LspInitResponse
instance ToJSON LspInitResponse
instance FromJSON LspResult
instance ToJSON LspResult
instance FromJSON ServerInfo
instance ToJSON ServerInfo

type LspResponseMergeError = String

-- | merge multiple LSP responses (of the same id) into one common response
mergeLspResponses :: [LSPR.LspResponse] -> Either LspResponseMergeError LSPR.LspResponse
mergeLspResponses = undefined -- TODO implement merging strategy | complex task

-- | Merge multiple LSP initialization messages
mergeLspInitMessages :: [T.Text] -> [Value] -> Maybe T.Text
mergeLspInitMessages agents initMsgs = do
    maybeObjs <- sequence (fmap maybeObject initMsgs)
    resultObjs <- sequence (fmap (\o -> parseMaybe (.: "result") o) maybeObjs)
    resultMaybeObjs <- sequence (fmap maybeObject resultObjs)
    capabilities <- sequence (fmap (\o -> parseMaybe (.: "capabilities") o) resultMaybeObjs)
    let mergedCapabilities = foldl' lodashMerge (Object mempty) (map Object capabilities)
    let serverInfo' = ServerInfo{name = (T.intercalate "/" agents), version = "1.0"}
    let lspResult = LspResult{capabilities = mergedCapabilities, serverInfo = serverInfo'}
    let lspInitResponse = LspInitResponse{id = 1, jsonrpc = "2.0", result = lspResult}

    pure $ TE.decodeUtf8 $ BL.toStrict $ encode lspInitResponse
