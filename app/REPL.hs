module REPL (
  module REPL
  ) where

import           Actions
import           Types

repl :: IO ()
repl = do
  putText "Enter your command: "
  content <- words <$> getLine
  let result = parse content
  case result of
      Quit     -> die "Dumb luck, motherducker"
      Go place -> do
        putTextLn $ situation place
        repl
  pure ()
