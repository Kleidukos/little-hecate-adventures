module Actions (
  module Actions
  ) where

import           Types

situation :: Text -> Text
situation description = "You are in " <> description

parse :: [Text] -> Action
parse content = case content of
                ["go", tag] -> Go tag
                _           -> Quit
