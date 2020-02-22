module Actions (
  module Actions
  ) where


import           Data.Maybe         (fromJust)
import           Text.Pretty.Simple (pPrint)

import           Types

parse :: [Text] -> Action
parse ["go", tag]  = Go tag
parse ["look"]     = Look
parse ["get", tag] = Get tag
parse ["dump"]     = Dump
parse _            = Abort

process :: StateIO m => Action -> m [Text]
process (Dump)  = get >>= pPrint >> pure []
process (Abort) = pure [ "I don't know what you mean…" ]
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
  objects <- listObjectsAt tag
  case objects of
    [] -> pure ["Nothing here…"]
    _  -> do
      let objectTexts = fmap (\obj -> "You see " <> description obj) objects
      pure $ ["You are in " <> currentLocationDescription] ++ objectTexts
process (Get tagName) = getItem tagName
--------------
-- Location --
--------------

listObjectsAt :: StateIO m => Tag -> m [Object]
listObjectsAt tagName = do
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
setLocation object to = do
  AppState{..} <- get
  let tagName   = tag object
  let newObject = object{location=(Just to)}
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

getItem :: StateIO m => Tag -> m [Text]
getItem tagName = do
  object <- getByTag tagName
  case object of
    Nothing  -> pure ["I don't understand what item you mean."]
    Just obj -> do
      myself <- fromJust <$> getByTag "yourself"
      moveObject obj (fromJust $ location myself)

moveObject :: StateIO m => Object -> Object -> m [Text]
moveObject object to = do
  setLocation object to
  pure ["OK."]
