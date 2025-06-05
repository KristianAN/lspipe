{-# LANGUAGE OverloadedStrings #-}

module Lsp.LspResponseSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as TE

import Data.Aeson
import Data.Text qualified as T
import Lsp.LspResponse (LspResponse (..), extractCapabilities)
import Test.Hspec

typoResponse = "{\"jsonrpc\":\"2.0\",\"result\":{\"capabilities\":{\"codeActionProvider\":{\"codeActionKinds\":[\"quickfix\"],\"workDoneProgress\":false},\"positionEncoding\":\"utf-16\",\"textDocumentSync\":1,\"workspace\":{\"workspaceFolders\":{\"changeNotifications\":true,\"supported\":true}}},\"serverInfo\":{\"name\":\"typos\",\"version\":\"0.1.37\"}},\"id\":1}"

decodeResp :: String -> Maybe LspResponse
decodeResp req = decode (BL.fromStrict (TE.encodeUtf8 (T.pack req)))

spec :: Spec
spec = do
    describe "Lsp.LspResponse.extractCapabilities" $ do
        it "can decode response" $ do
            fmap (\r -> jsonrpc r) (decodeResp typoResponse) `shouldBe` Just "2.0"

        it "should get all capability keys from initialize response" $ do
            let capability = case (decodeResp typoResponse) of
                    Just v -> extractCapabilities v
                    Nothing -> Nothing
            capability `shouldBe` Just (["codeActionProvider", "positionEncoding", "textDocumentSync", "workspace"])
