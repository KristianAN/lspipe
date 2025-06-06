{-# LANGUAGE OverloadedStrings #-}

module Lsp.MergerSpec (spec) where

import Data.Aeson
import Data.Text qualified as T
import Lsp.LspResponse (LspResponse (..), extractCapabilities)
import Test.Hspec

typoResponse = "{\"jsonrpc\":\"2.0\",\"result\":{\"capabilities\":{\"codeActionProvider\":{\"codeActionKinds\":[\"quickfix\"],\"workDoneProgress\":false},\"positionEncoding\":\"utf-16\",\"textDocumentSync\":1,\"workspace\":{\"workspaceFolders\":{\"changeNotifications\":true,\"supported\":true}}},\"serverInfo\":{\"name\":\"typos\",\"version\":\"0.1.37\"}},\"id\":1}"


spec :: Spec
spec = do
    describe "Lsp.Merger.mergeCapapilities" $ do
        it "can merge capabilities" $ do
          shouldBe True True

