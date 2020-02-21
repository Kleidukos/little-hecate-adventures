module Actions (
  module Actions
  ) where


import           Types

listContent :: StateIO m => Tag -> m [Object]
listContent tagName = do
  place <- gets (\state -> filter (\obj -> tag obj == tagName) (objects state))
  case place of
      [place] -> gets (\state ->
                        filter (\obj -> location obj == Just place ) (objects state))
      _       -> pure []

setCurrentLocation :: StateIO m => Tag -> m ()
setCurrentLocation tagName  = do
  location <- gets (\state -> filter (\location -> tag location == tagName) $ objects state)
  case location of
      [object] -> modify' (\state -> state { currentLocation = object })
      _        -> pure ()

process :: StateIO m => Action -> m [Text]
process (Abort)       = pure [ "I don't know what you mean…" ]
process (Go tagName) = do
    location <- gets (\state -> filter (\location -> tag location == tagName) $ objects state)
    case location of
        [loc] -> do
          AppState{..} <- get
          if currentLocation == loc
          then
            pure [ "You are already there." ]
          else do
            setCurrentLocation tagName
            process Look
        _ ->
          process Abort
process (Look)     = do
  Object{tag=tag, description=currentLocationDescription} <- gets currentLocation
  objects <- listContent tag
  case objects of
    [] -> do
      putTextLn "Nothing here…"
      pure $ []
    _  -> do
      let objectTexts = fmap (\obj -> "You see " <> description obj) objects
      pure $ ["You are in " <> currentLocationDescription] ++ objectTexts

parse :: [Text] -> Action
parse content = case content of
                ["go", tag] -> Go tag
                ["look"]    -> Look
                _           -> Abort
