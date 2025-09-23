{-# LANGUAGE OverloadedStrings #-}

module Lsp.Proxy where

import Control.Lens ((^.))
import Data.Foldable
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.STM
import Effectful.Error.Static
import Effectful.FileSystem.IO
import Language.LSP.Protocol.Lens (HasId (..), HasMethod (..))
import Language.LSP.Protocol.Message qualified as M
import Lsp.Agent qualified as A
import Lsp.Queues
import Lsp.Rpc (LspError (..), rpcRead)
import Util.Logger
import Prelude hiding (id)

--  Application dataflow Client Request Agent Response
-- The flow after Lspipe has started all agents, i.e. the request flow.
--  1. Client sends a request to lspipe
--  2. Lspipe checks which agents the requests will be fanned out to
--  3. Lspipe creates a PendingLspResponse with the initial request and the id of the agents it fans out to
--  4. Lspipe puts the PendingLspResponse into the AgentResponseBuffer
--  5. Lspipe fans out the requests to the agents by putting tagged requests in the ClientToAgentRequestQ
--  6. lspipe reads ClientToAgentsRequestQ and sends message to agents
--  7. The agents respond to lspipe, the responses are put into the AgentToClientResponseQ
--  8. Lspipe reads from the AgentToClientResponseQ and populates the AgentResponseBuffer.
--  9. Lspipe watches AgentResponseBuffer and merges the responses
--  10. Lspipe sends merged responses to the client

--  Application dataflow Agent Request Client Response

--  I. Agent sends request to lspipe
--  II. Lspipe puts request into ClientResponseBuffer
--  III. Lspipe puts request into AgentToClientRequestQ
--  IV. Lspipe reads AgentToClientRequestQ and sends the request to the client
--  V. client responds
--  VI. lspipe reads response gets the id from the ClientResponseBuffer
--  VII. lspipe puts tagged response into the ClientToAgentResponseQ
--  VIII. lspipe reads ClientToAgentResponseQ and responds to agents

-- Functions for putting data into request queues, all these functions involve rpcRead
handleClientToAgentRequest ::
    ( Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    ) =>
    [A.Agent] ->
    ClientToAgentsRequestQ ->
    AgentResponseBuffer ->
    M.RequestMessage ->
    Eff es ()
handleClientToAgentRequest agents cTaReqQ arb req = do
    addToAgentResponseBuffer arb initialPendingLspResp
    addToClientToAgentsRequestQ cTaReqQ agentTaggedLspRequest
  where
    requestId = req ^. id req
    requestMethod = req ^. method
    capableAgents = A.findCapableAgents req agents
    taggedAgents' = map (AgentId . A.agentId) capableAgents
    agentTaggedLspRequest = MkAgentTaggedRequest{taggedMessage = req, taggedAgents = taggedAgents'}
    initialPendingLspResp = MkPendingResponse{initialRequestId = requestId, initialMethod = requestMethod, agents = taggedAgents', responses = []}

handleClientToAgentResponse ::
    (Concurrent :> es) =>
    ClientToAgentResponseQ ->
    ClientResponseBuffer ->
    M.ResponseMessage ->
    Eff es ()
handleClientToAgentResponse cTaRespQ crb resp = do
    agentId' <- getAgentIdFromClientResponseBuffer crb resp
    addToClientToAgentsResponseQ cTaRespQ (taggedLspResponse agentId')
  where
    taggedLspResponse agentId' = MkAgentTaggedResponse{taggedResponse = resp, requestingAgent = agentId'}
clientToAgentMessageProcessor ::
    ( Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    , FileSystem :> es
    ) =>
    ClientHandles ->
    [A.Agent] ->
    ClientToAgentsRequestQ ->
    ClientToAgentResponseQ ->
    AgentResponseBuffer ->
    ClientResponseBuffer ->
    Eff es ()
clientToAgentMessageProcessor clientHandles agents cTaReqQ cTaRespQ arb crb = do
    message <- rpcRead $ inputHandle clientHandles
    _ <- case parseClientMessageType message of
        Right success -> case success of
            ClientRequest req -> handleClientToAgentRequest agents cTaReqQ arb req
            ClientNotification req -> undefined
            ClientResponse resp -> handleClientToAgentResponse cTaRespQ crb resp
        Left err -> throwError $ LspError $ "Unable to parse client message " <> (T.pack err)
    clientToAgentMessageProcessor clientHandles agents cTaReqQ cTaRespQ arb crb

-- whenever a new request is added to the AgentResponseBuffer this function is called and we check
processLspResponseBuffer ::
    ( Concurrent :> es
    , Error LspError :> es
    ) =>
    M.ResponseMessage ->
    AgentResponseBuffer ->
    AgentToClientResponseQ ->
    Eff es ()
processLspResponseBuffer resp arb aToCRQ = do
    pendingLspResponse <- getPendingResponse arb respId
    if completePendingResponse pendingLspResponse
        then do
            mergeAndQueueResponse pendingLspResponse aToCRQ
            removeFromBuffer arb respId
        else updatePendingRespInBuffer arb pendingLspResponse resp
  where
    respId = RequestId $ id resp

agentsToClientMessageProcessor ::
    ( FileSystem :> es
    , Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    ) =>
    [A.Agent] ->
    AgentResponseBuffer ->
    AgentToClientRequestQ ->
    AgentToClientResponseQ ->
    Eff es ()
agentsToClientMessageProcessor agents arb aToCReqQ aToCRespQ = do
    traverse_
        ( \agent -> do
            clientMessage <- rpcRead (A.stdout agent)
            case parseClientMessageType clientMessage of
                Right parsedClientMessage -> case parsedClientMessage of
                    ClientRequest request -> undefined
                    ClientNotification notification -> undefined
                    ClientResponse response -> undefined
                Left err -> throwError $ LspError $ "Unable to parse message from client: " <> T.pack err
                -- case parseMessage message of
                --     ParsedRequest req -> addToAgentToClientRequestQ req aToCReqQ
                --     ParsedResponse resp -> processLspResponseBuffer resp arb aToCRespQ
        )
        agents

    agentsToClientMessageProcessor agents arb aToCReqQ aToCRespQ

-- Functions for reading from queues are here, all these functions involve rpcWrite

processClientToAgentRequestQ :: ClientToAgentsRequestQ -> Eff es ()
processClientToAgentRequestQ = undefined

processAgentToClientResponseQ :: AgentToClientResponseQ -> Eff es ()
processAgentToClientResponseQ = undefined

processAgentToClientRequestQ :: AgentToClientRequestQ -> Eff es ()
processAgentToClientRequestQ = undefined

processCLientToAgentResponseQ :: ClientToAgentResponseQ -> Eff es ()
processCLientToAgentResponseQ = undefined
