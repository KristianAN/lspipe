module Lsp.Agent (LspAgent (..), method2capability) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful.FileSystem.IO (Handle)
import Effectful.Process (ProcessHandle)

data LspAgent = LspAgent
    { agentId :: T.Text
    , capabilities :: [T.Text]
    , stdin :: Handle
    , stdout :: Handle
    , procHandle :: ProcessHandle
    }

method2capability :: Map.Map String String
method2capability =
    Map.fromList
        [ -- notifications
          ("initialized", "*")
        , ("workspace/didChangeConfiguration", "*")
        , ("textDocument/didOpen", "textDocumentSync")
        , ("textDocument/didChange", "textDocumentSync")
        , ("textDocument/willSave", "textDocumentSync.willSave")
        , ("textDocument/didSave", "textDocumentSync.save")
        , ("textDocument/didClose", "textDocumentSync")
        , ("workspace/didCreateFiles", "workspace.fileOperations.didCreate")
        , ("workspace/willRenameFiles", "workspace.fileOperations.willRename")
        , ("workspace/didRenameFiles", "workspace.fileOperations.didRename")
        , ("workspace/willDeleteFiles", "workspace.fileOperations.willDelete")
        , ("workspace/didDeleteFiles", "workspace.fileOperations.didDelete")
        , ("workspace/didChangeWorkspaceFolders", "workspace.workspaceFolders.changeNotifications")
        , ("workspace/willCreateFiles", "workspace.fileOperations.willCreate")
        , -- requests
          ("textDocument/willSaveWaitUntil", "textDocumentSync.willSaveWaitUntil")
        , ("textDocument/declaration", "declarationProvider")
        , ("textDocument/definition", "definitionProvider")
        , ("textDocument/typeDefinition", "typeDefinitionProvider")
        , ("textDocument/implementation", "implementationProvider")
        , ("textDocument/references", "referencesProvider")
        , ("textDocument/prepareCallHierarchy", "callHierarchyProvider")
        , ("callHierarchy/incomingCalls", "callHierarchyProvider")
        , ("callHierarchy/outgoingCalls", "callHierarchyProvider")
        , ("textDocument/prepareTypeHierarchy", "typeHierarchyProvider")
        , ("typeHierarchy/supertypes", "textDocument/prepareTypeHierarchy")
        , ("typeHierarchy/subtypes", "textDocument/prepareTypeHierarchy")
        , ("textDocument/documentHighlight", "documentHighlightProvider")
        , ("textDocument/documentLink", "documentLinkProvider")
        , ("documentLink/resolve", "documentLinkProvider")
        , ("textDocument/hover", "hoverProvider")
        , ("textDocument/codeLens", "codeLensProvider")
        , ("codeLens/resolve", "codeLensProvider")
        , ("textDocument/foldingRange", "foldingRangeProvider")
        , ("textDocument/selectionRange", "selectionRangeProvider")
        , ("textDocument/documentSymbol", "documentSymbolProvider")
        , ("textDocument/semanticTokens/full", "semanticTokensProvider")
        , ("textDocument/semanticTokens/delta", "semanticTokensProvider")
        , ("textDocument/semanticTokens/range", "semanticTokensProvider")
        , ("textDocument/inlayHint", "inlayHintProvider")
        , ("inlayHint/resolve", "inlayHintProvider")
        , ("textDocument/inlineValue", "inlineValueProvider")
        , ("textDocument/moniker", "monikerProvider")
        , ("textDocument/completion", "completionProvider")
        , ("completionItem/resolve", "completionProvider")
        , ("textDocument/diagnostic", "diagnosticProvider")
        , ("workspace/diagnostic", "diagnosticProvider")
        , ("textDocument/signatureHelp", "signatureHelpProvider")
        , ("textDocument/codeAction", "codeActionProvider")
        , ("codeAction/resolve", "codeActionProvider")
        , ("textDocument/documentColor", "colorProvider")
        , ("textDocument/colorPresentation", "colorProvider")
        , ("textDocument/formatting", "documentFormattingProvider")
        , ("textDocument/rangeFormatting", "documentRangeFormattingProvider")
        , ("textDocument/onTypeFormatting", "documentOnTypeFormattingProvider")
        , ("textDocument/rename", "renameProvider")
        , ("textDocument/prepareRename", "renameProvider")
        , ("textDocument/linkedEditingRange", "linkedEditingRangeProvider")
        , ("workspace/symbol", "workspaceSymbolProvider")
        , ("workspaceSymbol/resolve", "workspaceSymbolProvider")
        , ("workspace/workspaceFolders", "workspace.workspaceFolders")
        , ("workspace/executeCommand", "executeCommandProvider")
        ]
