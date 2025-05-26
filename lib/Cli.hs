module Cli (Command (..)) where

import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO

data Command = Command {command :: String, args :: [String]}
