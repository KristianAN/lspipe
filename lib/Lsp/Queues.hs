{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lsp.Queues (
    getPendingResponse,
    completePendingResponse,
    mergeAndQueueResponse,
    removeFromBuffer,
    updatePendingRespInBuffer,
    ParsedClientMessage (..),
    AgentToClientResponseQ,
    AgentToClientRequestQ,
    parseClientMessageType,
    ClientToAgentResponseQ,
    ClientResponseBuffer,
    AgentTaggedResponse (..),
    AgentResponseBuffer,
    PendingResponse (..),
    ClientHandles (..),
    ClientToAgentsRequestQ,
    AgentTaggedRequest (..),
) where

import Data.Aeson (Result (..), Value, eitherDecode, eitherDecodeStrict', fromJSON, toJSON)
import Data.Aeson.Types (parse)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent.STM
import Effectful.FileSystem.IO
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types

-- Handles of the client, put in a data structure to more easily distinguish
data ClientHandles = MkClientHandles
    { inputHandle :: Handle
    , outputHandle :: Handle
    }

-- The ID of an agent. An agent is in most cases a LSP server
newtype AgentId = AgentId T.Text
    deriving (Show, Eq, Ord)

-- DataType to hold pending responses in the buffer
data PendingResponse = MkPendingResponse
    { initialRequestId :: LSP.SomeLspId -- The original request ID
    , initialMethod :: LSP.SomeClientMethod -- The method that was called
    , agents :: [AgentId] -- Agents that received the request
    , responses :: [LSP.FromServerMessage] -- Responses received so far
    }

-- Buffer for responses from agents
newtype AgentResponseBuffer = AgentResponseBuffer (TVar (Map.Map LSP.SomeLspId PendingResponse))

-- Maps client request IDs to the agent that should receive the response
newtype ClientResponseBuffer = ClientResponseBuffer (TVar (Map.Map LSP.SomeLspId AgentId))

-- A client request tagged with the agents it should be sent to
data AgentTaggedRequest = MkAgentTaggedRequest
    { taggedMessage :: LSP.FromClientMessage
    , taggedAgents :: [AgentId]
    }

-- Requests made from the client to the agents are put in this queue
newtype ClientToAgentsRequestQ = ClientToAgentsRequestQ (TQueue AgentTaggedRequest)

-- Merged responses to be sent to the client
newtype AgentToClientResponseQ = AgentToClientResponseQ (TQueue LSP.FromServerMessage)

-- Request from the agents to the client
data AgentRequest = MkAgentRequest
    { requestFromAgent :: LSP.FromServerMessage
    , requestingAgent :: AgentId
    }

newtype AgentToClientRequestQ = AgentToClientRequestQ (TQueue AgentRequest)

-- Response from client to a specific agent
data AgentTaggedResponse = MkAgentTaggedResponse
    { taggedResponse :: LSP.FromClientMessage -- This would be a response message
    , requestingAgent :: AgentId
    }

-- Responses from the client to the agents
newtype ClientToAgentResponseQ = ClientToAgentResponseQ (TQueue AgentTaggedResponse)

-- These are the actual message types we care about
data ParsedClientMessage
    = ClientRequest LSP.RequestMessage
    | ClientNotification LSP.NotificationMessage
    | ClientResponse LSP.ResponseMessage -- Response to a server request

data ParsedServerMessage
    = ServerRequest LSP.RequestMessage
    | ServerNotification LSP.NotificationMessage
    | ServerResponse LSP.ResponseMessage -- Response to a client request

-- Parse a raw client message into the specific message type
parseClientMessageType :: T.Text -> Either String ParsedClientMessage
parseClientMessageType message = do
    value <- eitherDecodeStrict' messageBytes :: Either String Value
    -- Try parsing as each type
    case fromJSON value :: Result LSP.RequestMessage of
        Success req -> Right (ClientRequest req)
        Error _ ->
            case fromJSON value :: Result LSP.NotificationMessage of
                Success notif -> Right (ClientNotification notif)
                Error _ ->
                    case fromJSON value :: Result LSP.ResponseMessage of
                        Success resp -> Right (ClientResponse resp)
                        Error err -> Left $ "Failed to parse client message: " <> err
  where
    messageBytes = encodeUtf8 message

-- Parse a raw server message into the specific message type
parseServerMessageType :: BSL.ByteString -> Either String ParsedServerMessage
parseServerMessageType bs = do
    value <- eitherDecode bs :: Either String Value
    -- Try parsing as each type
    case fromJSON value :: Result LSP.RequestMessage of
        Success req -> Right (ServerRequest req)
        Error _ ->
            case fromJSON value :: Result LSP.NotificationMessage of
                Success notif -> Right (ServerNotification notif)
                Error _ ->
                    case fromJSON value :: Result LSP.ResponseMessage of
                        Success resp -> Right (ServerResponse resp)
                        Error err -> Left $ "Failed to parse server message: " <> err

-- Parse a message from the client (for compatibility with existing code)
parseClientMsg :: BSL.ByteString -> Either String LSP.FromClientMessage
parseClientMsg bs = do
    value <- eitherDecode bs :: Either String Value
    let lookupFunc :: LSP.LookupFunc LSP.ServerToClient LSP.SMethod
        lookupFunc _ = Nothing
    case parse (LSP.parseClientMessage lookupFunc) value of
        Success msg -> Right msg
        Error err -> Left err

-- Parse a message from a server/agent (for compatibility with existing code)
parseServerMsg :: BSL.ByteString -> Either String LSP.FromServerMessage
parseServerMsg bs = do
    value <- eitherDecode bs :: Either String Value
    let lookupFunc :: LSP.LookupFunc LSP.ClientToServer LSP.SMethod
        lookupFunc _ = Nothing
    case parse (LSP.parseServerMessage lookupFunc) value of
        Success msg -> Right msg
        Error err -> Left err

-- Helper to check if a parsed client message needs a response
clientMessageNeedsResponse :: ParsedClientMessage -> Bool
clientMessageNeedsResponse = \case
    ClientRequest _ -> True
    ClientNotification _ -> False
    ClientResponse _ -> False

-- Helper to check if a parsed server message needs a response
serverMessageNeedsResponse :: ParsedServerMessage -> Bool
serverMessageNeedsResponse = \case
    ServerRequest _ -> True
    ServerNotification _ -> False
    ServerResponse _ -> False

-- Check if all expected responses have been received
completePendingResponse :: PendingResponse -> Bool
completePendingResponse pending =
    length (responses pending) == length (agents pending)

-- Create a new pending response entry
createPendingResponse :: LSP.SomeLspId -> LSP.SomeClientMethod -> [AgentId] -> PendingResponse
createPendingResponse reqId method agentList =
    MkPendingResponse
        { initialRequestId = reqId
        , initialMethod = method
        , agents = agentList
        , responses = []
        }

-- Add a response to a pending response
addResponseToPending :: PendingResponse -> LSP.FromServerMessage -> PendingResponse
addResponseToPending pending resp =
    pending{responses = resp : responses pending}

-- Merge multiple responses into one
mergeResponses :: LSP.SomeClientMethod -> [LSP.FromServerMessage] -> LSP.FromServerMessage
mergeResponses method responses = undefined

-- Create an error response for a given request
createErrorResponse :: LSP.SomeLspId -> LSPErrorCodes -> T.Text -> LSP.ResponseMessage
createErrorResponse (LSP.SomeLspId reqId) errorCode errorMsg =
    LSP.ResponseMessage
        "2.0"
        ( case reqId of
            LSP.IdInt i -> InL i
            LSP.IdString s -> InR (InL s)
        )
        Nothing
        ( Just $
            LSP.ResponseError
                (InL errorCode)
                errorMsg
                Nothing
        )

-- Create a successful response with null result
createNullResponse :: LSP.SomeLspId -> LSP.ResponseMessage
createNullResponse (LSP.SomeLspId reqId) =
    LSP.ResponseMessage
        "2.0"
        ( case reqId of
            LSP.IdInt i -> InL i
            LSP.IdString s -> InR (InL s)
        )
        (Just (toJSON Null))
        Nothing

-- Queue operations
mergeAndQueueResponse :: (Concurrent :> es) => PendingResponse -> AgentToClientResponseQ -> Eff es ()
mergeAndQueueResponse = undefined

removeFromBuffer :: (Concurrent :> es) => AgentResponseBuffer -> LSP.SomeLspId -> Eff es ()
removeFromBuffer = undefined

updatePendingRespInBuffer :: (Concurrent :> es) => AgentResponseBuffer -> PendingResponse -> LSP.FromServerMessage -> Eff es ()
updatePendingRespInBuffer = undefined

addToAgentResponseBuffer :: (Concurrent :> es) => AgentResponseBuffer -> PendingResponse -> Eff es ()
addToAgentResponseBuffer = undefined

getPendingResponse :: (Concurrent :> es) => AgentResponseBuffer -> LSP.SomeLspId -> Eff es PendingResponse
getPendingResponse = undefined

addToClientToAgentsRequestQ :: (Concurrent :> es) => ClientToAgentsRequestQ -> AgentTaggedRequest -> Eff es ()
addToClientToAgentsRequestQ = undefined

getAgentIdFromClientResponseBuffer :: (Concurrent :> es) => ClientResponseBuffer -> LSP.FromServerMessage -> Eff es AgentId
getAgentIdFromClientResponseBuffer = undefined

addToClientToAgentsResponseQ :: (Concurrent :> es) => ClientToAgentResponseQ -> AgentTaggedResponse -> Eff es ()
addToClientToAgentsResponseQ = undefined

addToAgentToClientRequestQ :: (Concurrent :> es) => LSP.FromServerMessage -> AgentToClientRequestQ -> Eff es ()
addToAgentToClientRequestQ = undefined
