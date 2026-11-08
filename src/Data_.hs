{-# LANGUAGE BlockArguments #-}

module Data_ where

import Raylib.Types.Core (Rectangle (Rectangle))
import Data.Functor ((<&>))
import Data.List ((!?))
import Types

tiles :: Int -> Maybe [(Char, Rectangle)]
tiles l = tiles' !? l <&> map \(id', col, row) -> (id', Rectangle (col * k) (row * k) k k)
  where
    k = tilesLen

tiles' :: [[(Char, Float, Float)]]
tiles' =
  map
    ( concat
      . zipWith (\i r -> zipWith (\j x -> (x, j, i)) [0 ..] r) [0 ..]
      . lines
    )
    [ "       1      \n\
      \1 11111111111 \n\
      \1 3           \n\
      \111111   11111\n\
      \          2   \n\
      \   111111111  ",
      "       1      \n\
      \1 11111111111 \n\
      \1         2   \n\
      \111111   11111\n\
      \          3   \n\
      \   111111111  ",
      "       1  2   \n\
      \1 11111111111 \n\
      \1         3   \n\
      \111111   11111\n\
      \              \n\
      \   111111111  ",
      "          3   \n\
      \1 11 11   111 \n\
      \    2  1      \n\
      \11  11  11   1\n\
      \              \n\
      \   11  1 11   ",
      "              \n\
      \              \n\
      \    3         \n\
      \11  11  11   1\n\
      \  1  1  1  21 \n\
      \   11  1 11   "
    ]

playerRad, tilesLen :: (Num a) => a
playerRad = 15
tilesLen = 100

keysint :: [Int]
keysint = concat [[0, 4, 5, 24, 25, 32, 39, 59, 61, 96], [44 .. 57], [65 .. 93], [256 .. 269], [280 .. 284], [290 .. 301], [320 .. 336], [340 .. 348]]

restart :: LevelData
restart = LevelData
  { _player =
      Player
        { _pos = undefined,
          _uvel = pure 0,
          _tvel = pure 0,
          _air = True
        },
    _lvlnum = 0
  }
