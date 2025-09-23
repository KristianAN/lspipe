{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lsp.Agent (Agent (..), fanOutToAgents, findCapableAgents) where

import Control.Lens ((^?), _Just)
import Data.Text qualified as T
import Effectful.FileSystem.IO (Handle)
import Effectful.Process (ProcessHandle)
import Language.LSP.Protocol.Capabilities
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types

-- Your existing Agent type remains the same
data Agent = Agent
    { agentId :: T.Text
    , capabilities :: ServerCapabilities -- Changed to use proper LSP type
    , stdin :: Handle
    , stdout :: Handle
    , procHandle :: ProcessHandle
    }

instance Show Agent where
    show (Agent aid _ _ _ _) =
        "Agent id: " <> T.unpack aid

instance Eq Agent where
    a == b = agentId a == agentId b

-- Check if an agent supports a specific method
agentSupportsMethod :: SMethod m -> Agent -> Bool
agentSupportsMethod method agent =
    case getMethodCapability method (capabilities agent) of
        Nothing -> False
        Just _ -> True

-- Extract the capability for a given method from ServerCapabilities
getMethodCapability :: SMethod m -> ServerCapabilities -> Maybe (ServerCapability m)
getMethodCapability method caps =
    caps ^? serverCapability method . _Just

-- Find agents capable of handling a specific client method
findCapableAgents :: SomeClientMethod -> [Agent] -> [Agent]
findCapableAgents (SomeClientMethod method) agents =
    filter (agentSupportsMethod method) agents

-- Fan out a request to capable agents
fanOutToAgents :: SomeClientMethod -> [Agent] -> Either String [Agent]
fanOutToAgents someMethod agents =
    case findCapableAgents someMethod agents of
        [] -> Left $ "No registered capability for method: " <> show someMethod
        capableAgents -> Right capableAgents

-- Helper to check if method requires special handling (like broadcast)
isUniversalMethod :: SomeClientMethod -> Bool
isUniversalMethod (SomeClientMethod method) =
    case method of
        SMethod_Initialized -> True
        SMethod_WorkspaceDidChangeConfiguration -> True
        _ -> False

-- Alternative implementation that handles universal methods
fanOutToAgentsWithUniversal :: SomeClientMethod -> [Agent] -> Either String [Agent]
fanOutToAgentsWithUniversal someMethod agents =
    if isUniversalMethod someMethod
        then Right agents -- Send to all agents
        else fanOutToAgents someMethod agents
