module Types (
  module Types
  ) where

data Location = Location { description :: Text
                         , tag         :: Text
                         } deriving (Show, Eq)

data Action = Go Text
            | Quit
            deriving (Show, Eq)

data AppState = AppState { stuff :: [Text]
                         } deriving (Eq, Show)

type StateIO m = (MonadState AppState m, MonadIO m)

exampleLocations :: [Location]
exampleLocations = [ Location "an open field" "field"
                   , Location "a little cave" "cave"
                   ]
