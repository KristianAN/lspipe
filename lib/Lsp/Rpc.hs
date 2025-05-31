{-# LANGUAGE OverloadedStrings #-}

module Lsp.Rpc (
    rpcRead,
    rpcWrite,
    LspError (..),
) where

import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Error.Static
import Effectful.Exception (try)
import Effectful.FileSystem
import Effectful.FileSystem.IO
import Effectful.FileSystem.IO.ByteString qualified as BS
import GHC.IO.Exception (IOException)
import System.IO.Error (ioeGetErrorString, isEOFError)
import Text.Read (readMaybe)

newtype LspError = LspError T.Text deriving (Show)

readBody :: (FileSystem :> es, Error LspError :> es) => Handle -> Int -> Eff es T.Text
readBody handle len = do
    eBodyBytes <- try @IOException (BS.hGet handle len)
    case eBodyBytes of
        Left e -> throwError $ LspError ("IO Error reading body: " <> T.pack (ioeGetErrorString e))
        Right bodyBytes ->
            case TE.decodeUtf8' bodyBytes of
                Left unicodeEx -> throwError $ LspError ("UTF-8 decoding error: " <> T.pack (show unicodeEx))
                Right bodyText -> pure bodyText

rpcRead :: (FileSystem :> es, Error LspError :> es) => Handle -> Eff es T.Text
rpcRead handle = recur Nothing
  where
    recur :: (FileSystem :> es, Error LspError :> es) => Maybe Int -> Eff es T.Text
    recur contentLength = do
        eLine <- try @IOException $ BS.hGetLine handle
        case eLine of
            Left e
                | isEOFError e -> pure T.empty
                | otherwise -> throwError $ LspError (T.pack "IO Error reading header:")
            Right lineStr ->
                if lineStr == "\r"
                    then case contentLength of
                        Just len -> readBody handle len
                        Nothing -> throwError $ LspError "Separator line (\\r\\n) encountered before Content-Length header"
                    else parseHeaderLine lineStr contentLength
    parseHeaderLine :: (FileSystem :> es, Error LspError :> es) => BSC.ByteString -> Maybe Int -> Eff es T.Text
    parseHeaderLine lineStr mContentLength = do
        let line = TE.decodeUtf8 lineStr
        let effectiveLine = fromMaybe line (T.stripSuffix (T.pack "\r") line) -- Remove trailing \r if present
        let (headerName, restOfHeader) = T.breakOn (T.pack ":") effectiveLine

        if T.null restOfHeader || not (T.isPrefixOf (T.pack ":") restOfHeader)
            then
                throwError $ LspError ("Invalid header format (missing or misplaced colon): " <> effectiveLine)
            else do
                let headerValue = T.strip (T.drop (T.length (T.pack ":")) restOfHeader)
                if T.toLower headerName == T.pack "content-length"
                    then case readMaybe (T.unpack headerValue) of
                        Just len
                            | len >= 0 -> recur (Just len)
                            | otherwise -> throwError $ LspError ("Invalid Content-Length (negative): " <> headerValue)
                        Nothing -> throwError $ LspError ("Invalid Content-Length value (not a number): " <> headerValue)
                    else
                        recur mContentLength

rpcWrite :: (FileSystem :> es, Error LspError :> es) => Handle -> T.Text -> Eff es ()
rpcWrite handle content = do
    let contentBytes = TE.encodeUtf8 content
    let contentLength = BSC.length contentBytes
    let headerLine = BSC.pack $ "Content-Length: " ++ show contentLength ++ "\r\n"
    let separatorLine = BSC.pack "\r\n"

    -- Actions that can throw IOException
    let ioActions = do
            BS.hPut handle headerLine
            BS.hPut handle separatorLine
            BS.hPut handle contentBytes
            hFlush handle

    eResult <- try @IOException ioActions
    case eResult of
        Left e -> throwError $ LspError ("IO Error writing message: " <> T.pack (show e)) -- 'e' is now IOException
        Right _ -> pure ()
