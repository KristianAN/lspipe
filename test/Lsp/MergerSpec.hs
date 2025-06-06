{-# LANGUAGE OverloadedStrings #-}

module Lsp.MergerSpec (spec) where

import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lsp.LspResponse (LspResponse (..), extractCapabilities)
import Lsp.Merger (mergeLspInitMessages)
import Test.Hspec

parseJson :: T.Text -> Value
parseJson jsonText = case decode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
    Just val -> val
    Nothing -> Prelude.error "Failed to parse JSON"

typoResponse = parseJson "{\"jsonrpc\":\"2.0\",\"result\":{\"capabilities\":{\"codeActionProvider\":{\"codeActionKinds\":[\"quickfix\"],\"workDoneProgress\":false},\"positionEncoding\":\"utf-16\",\"textDocumentSync\":1,\"workspace\":{\"workspaceFolders\":{\"changeNotifications\":true,\"supported\":true}}},\"serverInfo\":{\"name\":\"typos\",\"version\":\"0.1.37\"}},\"id\":1}"

hlsResponse = parseJson "{\"id\":1,\"jsonrpc\":\"2.0\",\"result\":{\"capabilities\":{\"callHierarchyProvider\":{\"workDoneProgress\":false},\"codeActionProvider\":{\"resolveProvider\":true,\"workDoneProgress\":false},\"codeLensProvider\":{\"resolveProvider\":true,\"workDoneProgress\":false},\"completionProvider\":{\"resolveProvider\":true,\"triggerCharacters\":[\".\"],\"workDoneProgress\":false},\"definitionProvider\":{\"workDoneProgress\":false},\"documentFormattingProvider\":{\"workDoneProgress\":false},\"documentHighlightProvider\":{\"workDoneProgress\":false},\"documentRangeFormattingProvider\":{\"workDoneProgress\":false},\"documentSymbolProvider\":{\"workDoneProgress\":false},\"executeCommandProvider\":{\"commands\":[\"8523:cabalHaskellIntegration:cabalAdd\",\"8523:ghcide-extend-import-action:extendImport\",\"8523:retrie:retrieCommand\",\"8523:retrie:retrieInlineThisCommand\",\"8523:explicit-fields:codeActionResolve\",\"8523:hlint:codeActionResolve\",\"8523:eval:evalCommand\",\"8523:ghcide-type-lenses:typesignature.add\",\"8523:gadt:GADT.toGADT\",\"8523:importLens:ImportLensCommand\",\"8523:class:classplugin.codeaction\",\"8523:class:classplugin.typelens\",\"8523:splice:expandTHSpliceInplace\",\"8523:moduleName:updateModuleName\"],\"workDoneProgress\":false},\"foldingRangeProvider\":{\"workDoneProgress\":false},\"hoverProvider\":{\"workDoneProgress\":false},\"implementationProvider\":{\"workDoneProgress\":false},\"inlayHintProvider\":{\"resolveProvider\":false,\"workDoneProgress\":false},\"positionEncoding\":\"utf-16\",\"referencesProvider\":{\"workDoneProgress\":false},\"renameProvider\":{\"prepareProvider\":true,\"workDoneProgress\":false},\"selectionRangeProvider\":{\"workDoneProgress\":false},\"semanticTokensProvider\":{\"full\":{\"delta\":true},\"legend\":{\"tokenModifiers\":[\"declaration\",\"definition\",\"readonly\",\"static\",\"deprecated\",\"abstract\",\"async\",\"modification\",\"documentation\",\"defaultLibrary\"],\"tokenTypes\":[\"namespace\",\"type\",\"class\",\"enum\",\"interface\",\"struct\",\"typeParameter\",\"parameter\",\"variable\",\"property\",\"enumMember\",\"event\",\"function\",\"method\",\"macro\",\"keyword\",\"modifier\",\"comment\",\"string\",\"number\",\"regexp\",\"operator\",\"decorator\"]},\"workDoneProgress\":false},\"textDocumentSync\":{\"change\":2,\"openClose\":true,\"save\":{}},\"typeDefinitionProvider\":{\"workDoneProgress\":false},\"workspace\":{\"workspaceFolders\":{\"changeNotifications\":true,\"supported\":true}},\"workspaceSymbolProvider\":{\"resolveProvider\":false,\"workDoneProgress\":false}}}}"

spec :: Spec
spec = do
    describe "Lsp.Merger.mergeCapapilities" $ do
        it "can merge capabilities" $ do
            shouldBe (mergeLspInitMessages ["hls", "typo"] [hlsResponse, typoResponse]) (Just "{\"fun\":true}")
