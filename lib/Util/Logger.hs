{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Util.Logger (Logger, LogLevel (..), logInfo, logError, logDebug, runConsoleLogger, runFileSystemLogger) where

import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effectful
import Effectful.Console.ByteString qualified as C
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem
import Effectful.FileSystem.IO
import Effectful.FileSystem.IO.ByteString

data Logger :: Effect where
    LogInfo :: T.Text -> Logger m ()
    LogError :: T.Text -> Logger m ()
    LogDebug :: T.Text -> Logger m ()

data LogLevel = Info | Debug | Error

logInfo :: (HasCallStack, Logger :> es) => T.Text -> Eff es ()
logInfo message = send $ LogInfo message

logError :: (HasCallStack, Logger :> es) => T.Text -> Eff es ()
logError message = send $ LogError message

logDebug :: (HasCallStack, Logger :> es) => T.Text -> Eff es ()
logDebug message = send $ LogDebug message

type instance DispatchOf Logger = Dynamic

logMessage :: (C.Console :> es, IOE :> es) => T.Text -> T.Text -> Eff es ()
logMessage level message = do
    timeStamp <- currentTime
    C.putStrLn $ encodeUtf8 $ createLogText timeStamp level message

createLogText :: T.Text -> T.Text -> T.Text -> T.Text
createLogText timestamp level message = "<" <> timestamp <> "> " <> level <> message

currentTime :: (IOE :> es) => Eff es T.Text
currentTime = do
    now <- liftIO (getCurrentTime :: IO UTCTime)
    pure $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

runConsoleLogger :: (C.Console :> es, IOE :> es) => Eff (Logger : es) a -> Eff es a
runConsoleLogger = interpret $ \_ -> \case
    LogInfo message -> logMessage "INFO: " message
    LogError message -> logMessage "ERROR: " message
    LogDebug message -> logMessage "DEBUG: " message

runFileSystemLogger :: (IOE :> es, FileSystem :> es) => LogLevel -> Eff (Logger : es) a -> Eff es a
runFileSystemLogger level l = do
    createDirectoryIfMissing True "/tmp/lspipe"
    interpret
        ( \_ -> \case
            LogInfo message -> case level of
                Error -> pure ()
                _ -> writeLogMessage "INFO: " message
            LogError message -> writeLogMessage "ERROR: " message
            LogDebug message -> case level of
                Debug -> writeLogMessage "DEBUG: " message
                _ -> pure ()
        )
        l

writeLogMessage :: (IOE :> es, FileSystem :> es) => T.Text -> T.Text -> Eff es ()
writeLogMessage level message = do
    ts <- currentTime
    withFile "/tmp/lspipe/lspipe.log" WriteMode $ \handle -> hPutStrLn handle (encodeUtf8 (createLogText ts level message))
