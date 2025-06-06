module Lsp.Merger (mergeCapabilities) where

import Data.Aeson
import Lsp.LspResponse qualified as LspR

results :: [LspR.LspResponse] -> [Value]
results = fmap LspR.result

mergeCapabilities :: [LspR.LspResponse] -> Value
mergeCapabilities = undefined
