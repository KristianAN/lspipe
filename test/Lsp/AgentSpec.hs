{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lsp.AgentSpec (spec) where

import Data.Aeson (Value (..))
import Data.Text qualified as T
import Lsp.Agent
import Lsp.LspRequest
import Test.Hspec
import Prelude hiding (id)

dummyHandle :: a
dummyHandle = undefined

dummyProcHandle :: a
dummyProcHandle = undefined

mkAgent :: T.Text -> [T.Text] -> LspAgent
mkAgent aid caps =
    LspAgent
        { agentId = aid
        , capabilities = caps
        , stdin = dummyHandle
        , stdout = dummyHandle
        , procHandle = dummyProcHandle
        }

mkRequest :: T.Text -> LspRequest
mkRequest method =
    LspRequest
        { jsonrpc = "2.0"
        , id = Just 1
        , method = method
        , params = Null
        }

spec :: Spec
spec = do
    describe "Lsp.Agent.fanOutToAgents" $ do
        it "fans out to correct agents when capability is present" $ do
            let
                req = mkRequest "textDocument/definition"
                agent1 = mkAgent "a1" ["definitionProvider", "hoverProvider"]
                agent2 = mkAgent "a2" ["hoverProvider"]
                agent3 = mkAgent "a3" ["definitionProvider"]
                agents = [agent1, agent2, agent3]
                result = fanOutToAgents req agents
            result `shouldBe` Right [agent1, agent3]

        it "returns Left when method is not registered" $ do
            let
                req = mkRequest "unknown/method"
                agent1 = mkAgent "a1" ["someProvider"]
                agents = [agent1]
                result = fanOutToAgents req agents
            result `shouldBe` Left "No registered capability for method: unknown/method"

        it "returns an empty list if no agent supports the capability" $ do
            let
                req = mkRequest "textDocument/definition"
                agent1 = mkAgent "a1" ["hoverProvider"]
                agent2 = mkAgent "a2" ["completionProvider"]
                agents = [agent1, agent2]
                result = fanOutToAgents req agents
            result `shouldBe` Right []

        it "returns all agents if all support the capability" $ do
            let
                req = mkRequest "textDocument/definition"
                agent1 = mkAgent "a1" ["definitionProvider"]
                agent2 = mkAgent "a2" ["definitionProvider", "hoverProvider"]
                agents = [agent1, agent2]
                result = fanOutToAgents req agents
            result `shouldBe` Right agents
