module Cli (runLspipe) where

import Effectful
import Effectful.Concurrent.Async
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.FileSystem.IO (stdin, stdout)
import Effectful.Process
import Lsp.Proxy (Command (..), runApp)
import Lsp.Rpc (LspError)
import Options.Applicative qualified as OA
import System.Exit (exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)
import Util.Logger

data Lspipe = Lspipe
    { servers :: [Command]
    }

lspipe :: OA.Parser Lspipe
lspipe = Lspipe <$> OA.many (fmap toCommand (OA.strOption (OA.long "server" <> OA.metavar "SERVER" <> OA.help "Servers to launch")))

toCommand :: String -> Command
toCommand input =
    let params = words input
     in case params of
            [] -> error "Empty server command"
            (cmd : []) -> Command{command = cmd, args = []}
            (cmd : args') -> Command{command = cmd, args = args'}

runLspipe :: IO ()
runLspipe = do
    options <- OA.execParser $ OA.info (lspipe OA.<**> OA.helper) OA.fullDesc
    result <- runEff $ runErrorNoCallStack @LspError $ runFileSystem $ (runFileSystemLogger Debug) $ runProcess $ runConcurrent $ do
        exitCode <- runApp stdin stdout (servers options)
        liftIO $ exitWith exitCode
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ show err
            exitFailure
        Right () -> pure ()
