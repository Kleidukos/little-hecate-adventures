module Main (main) where

import           REPL
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putTextLn "Welcome to Little Hécate Adventures!"
  repl
