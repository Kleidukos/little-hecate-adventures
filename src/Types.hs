module Types (
  module Types
  ) where

type Tag         = Text
type Description = Text

type StateIO m = (MonadState AppState m, MonadIO m)

data Action = Go Text
            | Abort
            | Look
            deriving (Show, Eq)

data AppState = AppState { currentLocation :: Object
                         , objects         :: [Object]
                         } deriving (Eq, Show)

data Object = Object { tag         :: Tag
                     , description :: Text
                     , location    :: Maybe Object
                     } deriving (Eq, Show)

initialState :: AppState
initialState = AppState defaultLoc objects
  where
    defaultLoc = Object "doorstep" "the doorstep" Nothing
    loc0 = Object "field" "an open field" Nothing
    loc1 = Object "cave" "a little cave" Nothing
    loc2 = Object "doorstep" "the doorstep" Nothing
    objects  =  [ loc0
                , loc1
                , loc2
                , Object "silver" "a silver coin" (Just loc0)
                , Object "gold" "a gold coin" (Just loc1)
                , Object "guard" "a burly guard" (Just loc0)
                ]

