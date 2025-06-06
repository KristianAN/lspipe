{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lsp.Proxy (runApp, Command (..)) where

import Data.Aeson qualified as Aeson

import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent.Async (async, waitCatch, withAsync)
import Effectful.Concurrent.STM
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hSetBuffering)
import Effectful.Process (CreateProcess (..), Process, StdStream (..), createProcess, proc, waitForProcess)
import Lsp.Agent qualified as Agent
import Lsp.LspRequest qualified as LspReq
import Lsp.LspResponse (LspResponseError (..))
import Lsp.LspResponse qualified as LspR
import Lsp.Merger (mergeLspInitMessages)
import Lsp.Rpc
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (BufferMode (BlockBuffering), Handle)
import Util.Logger

data Command = Command {command :: String, args :: [String]}

-- TODO Inject logic for which queue we write to based on the server capabilities
processChannelToWriterClient ::
    ( Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    , FileSystem :> es
    ) =>
    TQueue T.Text ->
    Handle ->
    Eff es ()
processChannelToWriterClient queue handle = do
    logDebug $ "starting processChannelToWriter with for client: "
    hSetBuffering handle (BlockBuffering Nothing)
    write
  where
    write = do
        msg <- atomically $ readTQueue queue
        logDebug $ "processChannelToWriterClient . got message from queue. will write with rpcWriter. message: " <> msg
        rpcWrite handle msg
        write

processChannelToWriterAgents ::
    ( Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    , FileSystem :> es
    ) =>
    TBQueue T.Text ->
    TVar [Agent.LspAgent] ->
    Eff es ()
processChannelToWriterAgents queue agents = do
    agents' <- atomically $ readTVar agents
    traverse_ (\a -> logDebug $ "starting processChannelToWriter with handle id: " <> (Agent.agentId a)) agents'
    traverse_ (\a -> hSetBuffering (Agent.stdin a) (BlockBuffering Nothing)) agents'
    write agents'
  where
    write agents' = do
        msg <- atomically $ readTBQueue queue
        logDebug $ "processChannelToWriterAgents got message from queue. will write with rpcWriter. message: " <> msg
        traverse_
            ( \writer -> do
                rpcWrite (Agent.stdin writer) msg
            )
            agents'
        write agents'

-- TODO do something more performant here
decodeLspRequest ::
    (Error LspError :> es) =>
    T.Text ->
    Eff es (Maybe LspReq.LspRequest)
decodeLspRequest req =
    case Aeson.decode (BL.fromStrict (TE.encodeUtf8 req)) of
        Just v -> pure (Just v)
        Nothing -> pure Nothing -- Could be a response, not a request

decodeLspResponse ::
    (Error LspError :> es) =>
    T.Text ->
    Eff es LspR.LspResponse
decodeLspResponse req =
    case Aeson.decode (BL.fromStrict (TE.encodeUtf8 req)) of
        Just v -> pure v
        Nothing -> throwError $ LspError "Unable to decode into LSP Response"

processClientReader ::
    ( Logger :> es
    , Error LspError :> es
    , Concurrent :> es
    , FileSystem :> es
    ) =>
    Handle -> -- Client's input
    TBQueue T.Text -> -- Queue for messages TO the LSP server
    TQueue T.Text -> -- Queue for messages TO the client (for "Server Busy" response)
    Eff es ()
processClientReader reader serverTBQueue clientTQueue = do
    logDebug "Setting up client reader handle"
    hSetBuffering reader (BlockBuffering Nothing)
    logDebug "Starting process loop for client"
    processLoop
  where
    processLoop = do
        message <- rpcRead reader
        if T.null message
            then do
                logInfo "Client reached EOF, detaching and shutting down"
                pure () -- End of stream
            else do
                maybeLspRequest <- decodeLspRequest message
                case maybeLspRequest of
                    Just lspRequest -> do
                        -- It's a request from the client
                        wroteSuccessfully <- atomically $ do
                            full <- isFullTBQueue serverTBQueue
                            if full
                                then pure False
                                else do
                                    writeTBQueue serverTBQueue message
                                    pure True
                        if wroteSuccessfully
                            then logDebug "wrote message from client to queue" >> processLoop
                            else handleFullQueue lspRequest message
                    Nothing -> do
                        -- TODO: This will not work properly with multiple agents (who gets the message?)
                        -- It's likely a response from the client, forward it directly
                        logDebug "message is not a request (likely a response), forwarding to server"
                        atomically $ writeTBQueue serverTBQueue message
                        processLoop

    handleFullQueue request message =
        case LspReq.id request of
            Nothing -> do
                logDebug "message had no ID, putting back into queue"
                atomically $ writeTBQueue serverTBQueue message
                processLoop
            Just reqId -> do
                let lspResponse =
                        LspR.LspResponse
                            { LspR.jsonrpc = LspReq.jsonrpc request
                            , LspR.id = Just reqId
                            , LspR.result = Aeson.Null
                            , LspR.error = Just (LspResponseError{code = -32803, message = T.pack "[lspipe] : Server is busy"})
                            }
                logDebug "message had id, server was busy, sending busy message to client"
                atomically $ writeTQueue clientTQueue (TE.decodeUtf8 (BL.toStrict (Aeson.encode lspResponse)))
                processLoop

-- TODO here we need to merge the responses before we send them to the client
processServerReader ::
    ( Logger :> es
    , Error LspError :> es
    , FileSystem :> es
    , Concurrent :> es
    ) =>
    TVar [Agent.LspAgent] ->
    TQueue T.Text ->
    Eff es ()
processServerReader agents queue = do
    logDebug "Setting up server reader"
    agents' <- readTVarIO agents
    logDebug "Fetching capabilities"
    (agentsWithCapabilities, initMsgs) <-
        ( traverse
                ( \agent -> do
                    hSetBuffering (Agent.stdout agent) (BlockBuffering Nothing)
                    initMsg <- rpcRead (Agent.stdout agent)
                    logDebug $ "init message from server is :" <> initMsg
                    response <- decodeLspResponse initMsg
                    messageJson <- case Aeson.decode (BL.fromStrict (TE.encodeUtf8 initMsg)) of
                        Just v -> pure v
                        Nothing -> throwError $ LspError "Unable to create Value from message"
                    capabilities <- case LspR.extractCapabilities response of
                        Just v -> pure v
                        Nothing -> throwError $ LspError "Unable to get server capabilities"
                    pure $ ((agent{Agent.capabilities = capabilities}, (messageJson)))
                )
                agents'
            )
            <&> unzip

    merged <- case mergeLspInitMessages (fmap Agent.agentId agents') initMsgs of
        Just a -> pure a
        Nothing -> throwError $ LspError "Unable to merge lsp init responses"
    atomically $ writeTQueue queue merged
    atomically $ writeTVar agents agentsWithCapabilities
    logDebug "Setting up server reader processLoop"
    traverse_ (\agent -> async (process (Agent.stdout agent) (Agent.agentId agent))) agentsWithCapabilities
  where
    process reader agentId = do
        message <- rpcRead reader
        if T.null message
            then do
                logInfo $ "LSP server " <> agentId <> " disconnected (EOF)"
                pure ()
            else do
                logDebug $ "got message from LSP server " <> agentId <> " writing to queue"
                atomically $ writeTQueue queue message
                process reader agentId

runApp ::
    ( Logger :> es
    , FileSystem :> es
    , Process :> es
    , Concurrent :> es
    , Error LspError :> es
    ) =>
    Handle ->
    Handle ->
    [Command] ->
    Eff es ExitCode
runApp clientReader clientWriter commands = do
    logInfo "starting lspipe"
    agents <- (traverse setupAgent commands) >>= newTVarIO
    let desiredCapacity = 128 :: Int
    clientToServerChan <- atomically $ newTBQueue (fromIntegral desiredCapacity)
    serverToClientChan <- atomically newTQueue
    withAsync (processChannelToWriterAgents clientToServerChan agents) $ \a1 ->
        withAsync (processServerReader agents serverToClientChan) $ \a2 ->
            withAsync (processChannelToWriterClient serverToClientChan clientWriter) $ \a3 ->
                withAsync (processClientReader clientReader clientToServerChan serverToClientChan) $ \a4 -> do
                    logDebug "waiting for async tasks to complete"
                    result <- (traverse waitCatch [a1, a2, a3, a4]) <&> sequence
                    logDebug "waiting for agents to exit"
                    agentsProcHandles <-
                        atomically (readTVar agents) >>= \agents' ->
                            traverse (async . waitForProcess . Agent.procHandle) agents'
                    traverse_ waitCatch agentsProcHandles
                    case result of
                        Left e -> do
                            logError ("Error in communication channels: " <> T.pack (show e))
                            pure (ExitFailure 1)
                        Right _ -> pure ExitSuccess

setupAgent :: (Error LspError :> es, Process :> es) => Command -> Eff es Agent.LspAgent
setupAgent serverCommand = do
    (maybeStdin, maybeStdout, _, procHandle) <-
        createProcess
            (proc (command serverCommand) (args serverCommand))
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }

    stdIn <- case maybeStdin of
        Just h -> pure h
        Nothing -> throwError $ LspError "Failed to get stdin handle when starting lsp"
    stdOut <- case maybeStdout of
        Just h -> pure h
        Nothing -> throwError $ LspError "Failed to get stdout handle when starting lsp"
    pure
        Agent.LspAgent
            { Agent.stdout = stdOut
            , Agent.stdin = stdIn
            , Agent.capabilities = []
            , Agent.agentId = T.pack (command serverCommand)
            , Agent.procHandle = procHandle
            }
