module Main (main) where

import Control.Monad.State (evalStateT)
import Elm (init', tick, tilClose)
import Raylib.Core as R (closeWindow, initWindow, setTargetFPS)

main :: IO ()
main = do
  setTargetFPS 30
  window <- initWindow 1000 1000 "hello"
  init'' <- init'
  evalStateT (tilClose tick) init''
  closeWindow (Just window)
