module Types (
  module Types
  ) where

import qualified Data.Map as Map

type Tag         = Text
type Description = Text
type Location    = Map Tag Description

type StateIO m = (MonadState AppState m, MonadIO m)

data Action = Go Text
            | Quit
            | Look
            deriving (Show, Eq)

data AppState = AppState { currentLocation :: (Tag, Description)
                         , locations       :: Location
                         } deriving (Eq, Show)

initialState :: AppState
initialState = AppState defaultLoc locations
  where
    defaultLoc = ("the doorstep", "doorstep")
    locations  = Map.fromList [ ("field", "an open field")
                              , ("cave", "a little cave")
                              , ("doorstep", "the doorstep")
                              ]

parse :: [Text] -> Action
parse content = case content of
                ["go", tag] -> Go tag
                ["look"]    -> Look
                _           -> Quit

