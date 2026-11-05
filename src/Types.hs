{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Raylib.Types.Core
import Data.Map
import Raylib.Types.Core.Textures

data Player = Player {
  _pos :: Vector2,
  _uvel :: Vector2,
  _tvel :: Vector2,
  _air :: Bool
} deriving Show
makeLenses ''Player

data LevelData = LevelData {
  _player :: Player,
  _image :: (Texture, Map Char Texture),
  _lvlnum :: Int
}
makeLenses ''LevelData

data Model =
  Level LevelData
  | Select
makeLenses ''Model
makePrisms ''Model

data Action =
  AStart | AEnd
  | AKey KeyboardKey
