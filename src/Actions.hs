module Actions (
  module Actions
  ) where


import           Types
import Data.Maybe (fromJust)

parse :: [Text] -> Action
parse content = case content of
                ["go", tag] -> Go tag
                ["look"]    -> Look
                _           -> Abort

process :: StateIO m => Action -> m [Text]
process (Abort)       = pure [ "I don't know what you mean…" ]
process (Go tagName) = do
    location <- getByTag tagName
    case location of
        Just loc -> do
          currentLoc <- getCurrentLocation
          if currentLoc == loc
          then
            pure [ "You are already there." ]
          else do
            setCurrentLocation loc
            process Look
        Nothing ->
          process Abort
process (Look)     = do
  Object{tag=tag, description=currentLocationDescription} <- getCurrentLocation
  objects <- listContent tag
  case objects of
    [] -> do
      putTextLn "Nothing here…"
      pure $ []
    _  -> do
      let objectTexts = fmap (\obj -> "You see " <> description obj) objects
      pure $ ["You are in " <> currentLocationDescription] ++ objectTexts


--------------
-- Location --
--------------

listContent :: StateIO m => Tag -> m [Object]
listContent tagName = do
  place <- getByTag tagName
  case place of
      Nothing    -> pure []
      Just place -> gets (\state ->
          filter (\obj -> location obj == Just place && tag obj /= "yourself" ) (objects state))

setCurrentLocation :: StateIO m => Location -> m ()
setCurrentLocation location = do
  myself   <- fromJust <$> getByTag "yourself"
  setLocation myself location
  pure ()

getCurrentLocation :: StateIO m => m Object
getCurrentLocation = do 
  myself <- fromJust <$> getByTag "yourself"
  pure $ fromJust $ location myself 

setLocation :: StateIO m => Object -> Location -> m ()
setLocation object newLocation = do
  AppState{..} <- get
  let tagName   = tag object
  let newObject = object{location=(Just newLocation)}
  let newListOfObjects = insertObject newObject $ deleteObject tagName objects
  modify' (\state -> state { objects = newListOfObjects })

deleteObject :: Tag -> [Object] -> [Object]
deleteObject tagName objectList =
  filter (\obj -> tag obj /= tagName) objectList

insertObject :: Object -> [Object] -> [Object]
insertObject object listOfObjects = object : listOfObjects

getByTag :: StateIO m => Tag -> m (Maybe Object)
getByTag tagName = listToMaybe
               <$> gets (\state -> filter (\location -> tag location == tagName) (objects state))

---------------
-- Inventory --
---------------

getItem :: StateIO m => Tag -> m ()
getItem tagName = undefined

moveObject :: StateIO m => Object -> Object -> Maybe Object -> m [Text]
moveObject object from to = do
  case (Just from) /= location object of
    True  -> pure ["You cannot do that."]
    False ->
      case to of
        Nothing  -> pure ["There is nobody here to give that to."]
        Just newLoc -> do
          setLocation object newLoc
          pure ["OK."]
