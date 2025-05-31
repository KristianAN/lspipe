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
import Effectful.Concurrent.Async (withAsync)
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
processChannelToWriter :: (Error LspError :> es, Concurrent :> es, FileSystem :> es) => MessageQueue T.Text -> [Handle] -> Eff es ()
processChannelToWriter queue handles = do
    traverse_ (\handle -> hSetBuffering handle (BlockBuffering Nothing)) handles
    write
  where
    readFromWrappedQueue :: MessageQueue T.Text -> STM T.Text
    readFromWrappedQueue aq = case aq of
        UnboundedQueue tq -> readTQueue tq
        BoundedQueue tbq -> readTBQueue tbq
    write = do
        msg <- atomically $ readFromWrappedQueue queue
        traverse_
            ( \writer -> do
                rpcWrite writer msg
            )
            handles
        write

decodeLspRequest :: (Error LspError :> es) => T.Text -> Eff es LspReq.LspRequest
decodeLspRequest req =
    case Aeson.decode (BL.fromStrict (TE.encodeUtf8 req)) of
        Just v -> pure v
        Nothing -> throwError $ LspError (T.pack "Unable to decode request as json")

proccessClientReader ::
    (Error LspError :> es, Concurrent :> es, FileSystem :> es) =>
    Handle -> -- Client's input
    TBQueue T.Text -> -- Queue for messages TO the LSP server
    TQueue T.Text -> -- Queue for messages TO the client (for "Server Busy" response)
    Eff es ()
proccessClientReader reader serverTBQueue clientTQueue = do
    hSetBuffering reader (BlockBuffering Nothing)
    processLoop
  where
    processLoop = do
        message <- rpcRead reader
        if T.null message
            then pure () -- End of stream
            else do
                lspRequest <- decodeLspRequest message
                wroteSuccessfully <- atomically $ do
                    full <- isFullTBQueue serverTBQueue
                    if full
                        then pure False
                        else do
                            writeTBQueue serverTBQueue message
                            pure True
                if wroteSuccessfully
                    then processLoop
                    else do handleFullQueue lspRequest message

    handleFullQueue request message =
        case LspReq.id request of
            Nothing -> do
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
                atomically $ writeTQueue clientTQueue (TE.decodeUtf8 (BL.toStrict (Aeson.encode lspResponse)))
                processLoop

-- TODO here we need to merge the responses before we send them to the client
processServerReader :: (Error LspError :> es, FileSystem :> es, Concurrent :> es) => [Agent.LspAgent] -> TQueue T.Text -> Eff es ()
processServerReader agents queue = do
    traverse_ (\agent -> hSetBuffering (Agent.stdout agent) (BlockBuffering Nothing)) agents
    traverse_ (process . Agent.stdout) agents
  where
    process reader = do
        message <- rpcRead reader
        if T.null message
            then pure ()
            else do
                atomically $ writeTQueue queue message
                process reader

runApp :: (Logger :> es, FileSystem :> es, Process :> es, Concurrent :> es, Error LspError :> es) => Handle -> Handle -> [Command] -> Eff es ExitCode
runApp clientReader clientWriter commands = do
    logInfo "starting lspipe"
    agents <- traverse setupAgent commands
    let desiredCapacity = 128 :: Int
    clientToServerChan <- atomically $ newTBQueue (fromIntegral desiredCapacity)
    serverToClientChan <- atomically newTQueue
    withAsync (processChannelToWriter (BoundedQueue clientToServerChan) (fmap Agent.stdin agents)) $ \_a1 ->
        withAsync (processChannelToWriter (UnboundedQueue serverToClientChan) [clientWriter]) $ \_a2 ->
            withAsync (processServerReader agents serverToClientChan) $ \_a3 ->
                withAsync (proccessClientReader clientReader clientToServerChan serverToClientChan) $ \_a4 -> do
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
