module REPL (
  module REPL
  ) where

import           Actions
import           Types

repl :: StateIO m => m ()
repl = do
  putText "--> "
  content <- words <$> getLine
  let result = parse content
  res <- process result
  traverse_ putTextLn res
  repl
  pure ()

