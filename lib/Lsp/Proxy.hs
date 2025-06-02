{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}

module Lsp.Proxy (runApp, Command (..)) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent.Async (async, wait)
import Effectful.Concurrent.STM
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hSetBuffering)
import Effectful.Process (CreateProcess (..), Process, StdStream (..), createProcess, proc, waitForProcess)
import Lsp.Agent qualified as Agent
import Lsp.LspRequest qualified as LspReq
import Lsp.LspResponse (LspResponseError (..))
import Lsp.LspResponse qualified as LspR
import Lsp.Rpc
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (BlockBuffering), Handle)
import Util.Logger

data Command = Command {command :: String, args :: [String]}

data MessageQueue a = BoundedQueue (TBQueue a) | UnboundedQueue (TQueue a)

-- TODO Inject logic for which queue we write to based on the server capabilities
processChannelToWriter :: (Logger :> es, Error LspError :> es, Concurrent :> es, FileSystem :> es) => MessageQueue T.Text -> T.Text -> [Handle] -> Eff es ()
processChannelToWriter queue handleID handles = do
    logDebug $ "starting processChannelToWriter with handle id: " <> handleID
    traverse_ (\handle -> hSetBuffering handle (BlockBuffering Nothing)) handles
    write
  where
    readFromWrappedQueue :: MessageQueue T.Text -> STM T.Text
    readFromWrappedQueue aq = case aq of
        UnboundedQueue tq -> readTQueue tq
        BoundedQueue tbq -> readTBQueue tbq
    write = do
        msg <- atomically $ readFromWrappedQueue queue
        logDebug $ "processChannelToWriter - " <> handleID <> ". got message from queue. will write with rpcWriter. message: " <> msg
        traverse_
            ( \writer -> do
                rpcWrite writer msg
            )
            handles
        write

-- TODO do something more performant here
decodeLspRequest :: (Error LspError :> es) => T.Text -> Eff es (Maybe LspReq.LspRequest)
decodeLspRequest req =
    case Aeson.decode (BL.fromStrict (TE.encodeUtf8 req)) of
        Just v -> pure (Just v)
        Nothing -> pure Nothing -- Could be a response, not a request

proccessClientReader ::
    (Logger :> es, Error LspError :> es, Concurrent :> es, FileSystem :> es) =>
    Handle -> -- Client's input
    TBQueue T.Text -> -- Queue for messages TO the LSP server
    TQueue T.Text -> -- Queue for messages TO the client (for "Server Busy" response)
    Eff es ()
proccessClientReader reader serverTBQueue clientTQueue = do
    logDebug "Setting up client reader handle"
    hSetBuffering reader (BlockBuffering Nothing)
    logDebug "Starting process loop for client"
    processLoop
  where
    processLoop = do
        message <- rpcRead reader
        logDebug $ "got message from client: " <> message
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
                            , LspR.id = reqId
                            , LspR.result = Aeson.Null
                            , LspR.error = Just (LspResponseError{code = -32803, message = T.pack "[lspipe] : Server is busy"})
                            }
                logDebug "message had id, server was busy, sending busy message to client"
                atomically $ writeTQueue clientTQueue (TE.decodeUtf8 (BL.toStrict (Aeson.encode lspResponse)))
                processLoop

-- TODO here we need to merge the responses before we send them to the client
processServerReader :: (Logger :> es, Error LspError :> es, FileSystem :> es, Concurrent :> es) => [Agent.LspAgent] -> TQueue T.Text -> Eff es ()
processServerReader agents queue = do
    logDebug "Setting up server reader"
    traverse_ (\agent -> hSetBuffering (Agent.stdout agent) (BlockBuffering Nothing)) agents
    logDebug "Setting up server reader processLoop"
    traverse_ (\agent -> async (process (Agent.stdout agent) (Agent.agentId agent))) agents
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

runApp :: (Logger :> es, FileSystem :> es, Process :> es, Concurrent :> es, Error LspError :> es) => Handle -> Handle -> [Command] -> Eff es ExitCode
runApp clientReader clientWriter commands = do
    logInfo "starting lspipe"
    agents <- traverse setupAgent commands
    let desiredCapacity = 128 :: Int
    clientToServerChan <- atomically $ newTBQueue (fromIntegral desiredCapacity)
    serverToClientChan <- atomically newTQueue
    asyncHandles <-
        traverse
            async
            [ processChannelToWriter (BoundedQueue clientToServerChan) "LSP AGENTS" (fmap Agent.stdin agents)
            , processChannelToWriter (UnboundedQueue serverToClientChan) "LSP CLIENT" [clientWriter]
            , processServerReader agents serverToClientChan
            , proccessClientReader clientReader clientToServerChan serverToClientChan
            ]

    logDebug "waiting for async tasks to complete"
    traverse_ wait asyncHandles
    logDebug "waiting for agents to exit"
    traverse (waitForProcess . Agent.procHandle) agents >> pure ExitSuccess -- TODO find some way to handle the exit code properly

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
