module REPL (
  module REPL
  ) where

import qualified Data.Map as Map
import           Types

repl :: StateIO m => m ()
repl = do
  putText "Enter your command: "
  content <- words <$> getLine
  let result = parse content
  res <- process result
  putTextLn res
  repl
  pure ()

process :: StateIO m => Action -> m Text
process (Quit)     = pure $ "I don't know what you mean…"
process (Go tag) = do
                      location <- gets (\state -> Map.lookup tag (locations state))
                      case location of
                          Just description -> do
                            setCurrentLocation tag description
                            process Look
                          Nothing ->
                            process Quit
process (Look)     = do
                      (_, description) <- gets currentLocation
                      pure $ "You are in " <> description

setCurrentLocation :: StateIO m => Tag -> Description -> m ()
setCurrentLocation tag description =
  modify' (\state -> state{ currentLocation = (tag, description) })

