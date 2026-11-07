{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses, makePrisms)
import Data.Map (Map)
import Raylib.Types.Core (KeyboardKey, Vector2)
import Raylib.Types.Core.Textures (Texture)

data Player = Player
  { _pos :: Vector2,
    _uvel :: Vector2,
    _tvel :: Vector2,
    _air :: Bool
  }
  deriving (Show)

makeLenses ''Player

data Persistent = Persistent
  { _ptex :: Texture,
    _tiletex :: Map Char Texture
  }

makeLenses ''Persistent

data LevelData = LevelData
  { _player :: Player,
    _lvlnum :: Int
  }

makeLenses ''LevelData

data Model
  = Level LevelData Persistent
  | Select Persistent

makeLenses ''Model
makePrisms ''Model

data Action
  = AStart
  | AEnd
  | AKey KeyboardKey
