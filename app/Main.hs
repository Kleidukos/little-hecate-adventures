module Main (main) where

import           REPL
import           System.IO
import           Types

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putTextLn "Welcome to Little Hécate Adventures!"
  evalStateT repl initialState
