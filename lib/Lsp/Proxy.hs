{-# LANGUAGE OverloadedStrings #-}

module Lsp.Proxy (runApp) where

import Cli (Command (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent.Async (withAsync)
import Effectful.Concurrent.STM
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (hSetBuffering, stdin, stdout)
import Effectful.Process (CreateProcess (..), Process, StdStream (..), createProcess, proc, waitForProcess)
import Lsp.LspRequest qualified as LspReq
import Lsp.LspResponse (LspResponseError (..))
import Lsp.LspResponse qualified as LspR
import Lsp.Rpc
import System.Exit (ExitCode)
import System.IO (BufferMode (BlockBuffering), Handle)

processChannelToWriterUnbounded :: (Error LspError :> es, Concurrent :> es, FileSystem :> es) => TQueue T.Text -> Handle -> Eff es ()
processChannelToWriterUnbounded queue writer = do
    hSetBuffering writer (BlockBuffering Nothing)
    write
  where
    write = do
        msg <- atomically $ readTQueue queue
        rpcWrite writer msg
        write

processChannelToWriterBounded :: (Error LspError :> es, Concurrent :> es, FileSystem :> es) => TBQueue T.Text -> Handle -> Eff es ()
processChannelToWriterBounded queue writer = do
    hSetBuffering writer (BlockBuffering Nothing)
    write
  where
    write = do
        msg <- atomically $ readTBQueue queue
        rpcWrite writer msg
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

processServerReader :: (Error LspError :> es, FileSystem :> es, Concurrent :> es) => Handle -> TQueue T.Text -> Eff es ()
processServerReader reader queue = do
    hSetBuffering reader (BlockBuffering Nothing)
    process
  where
    process = do
        message <- rpcRead reader
        if T.null message
            then pure ()
            else do
                atomically $ writeTQueue queue message
                process

runApp :: (FileSystem :> es, Process :> es, Concurrent :> es, Error LspError :> es) => Handle -> Handle -> Command -> Eff es ExitCode
runApp clientReader clientWriter serverCommand = do
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
    let desiredCapacity = 128 :: Int
    clientToServerChan <- atomically $ newTBQueue (fromIntegral desiredCapacity)
    serverToClientChan <- atomically newTQueue
    withAsync (processChannelToWriterBounded clientToServerChan stdIn) $ \_a1 ->
        withAsync (processChannelToWriterUnbounded serverToClientChan clientWriter) $ \_a2 ->
            withAsync (processServerReader stdOut serverToClientChan) $ \_a3 ->
                withAsync (proccessClientReader clientReader clientToServerChan serverToClientChan) $ \_a4 -> do
                    waitForProcess procHandle

runServer :: (FileSystem :> es, Error LspError :> es, Concurrent :> es, Process :> es) => [Command] -> Eff es ExitCode
runServer commands = do
    runApp stdin stdout (head commands)
