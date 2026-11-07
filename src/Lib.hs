{-# LANGUAGE LexicalNegation, MultiWayIf, BlockArguments, ScopedTypeVariables, RecordWildCards #-}
module Lib where

import Data_ ( playerRad, tiles, tiles', tilesLen )
import Types
import Utils

import Raylib.Core.Textures as Texture
import Raylib.Util.Colors
import Raylib.Util.Lenses
import Raylib.Util.GUI

import Raylib.Types.Core
import Raylib.Types.Core.Camera
import Raylib.Types.Core.Textures as Textures

import Linear
import Control.Monad
import Control.Lens
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe

-- io

squaretexture :: Int -> String -> IO Texture
squaretexture l = loadImage >=> (\p -> imageResize p l l) >=> loadTextureFromImage

-- thanks to https://discord.com/users/1276891525451743295
drawtiles :: [(Char, Rectangle)] -> M.Map Char Texture -> IO ()
drawtiles t i =
  for_ t (\(id', x) ->
    for_ (M.lookup id' i) (\i' ->
      drawTextureV i' (V2 (x^._rectangle'x) (x^._rectangle'y)) white))

drawplayer :: Vector2 -> Texture -> IO ()
drawplayer v i = drawTextureV i (v - pure playerRad) white

drawgui :: (Float, Float) -> IO (Maybe Int)
drawgui (w, h) = do
  let m = length tiles'
  let ah = h / fromIntegral m
  clicks <- forM [0..m - 1] \x -> do
    let xf = fromIntegral x
    clicked <- guiButton
      Rectangle {
        rectangle'x = 0,
        rectangle'y = xf * ah,
        rectangle'width = w,
        rectangle'height = ah
      }
      (Just (show (succ x)))
    return if clicked then Just x else Nothing
  return $ listToMaybe $ catMaybes clicks

-- pure

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Int -> Player -> Player
move l m@Player {_pos = p, _uvel = v, _tvel = t, _air = a} =
  m {_pos = p' + t', _tvel = t', _air = a'}
  where
    t1 = (v + V2 0 1 +) $ t & _x %~ (* 0.7)
    (p', t', a') = foldr block (p, t1, a) [y | (x, y) <-  tiles l, x == '1']

block :: Rectangle -> (Vector2, Vector2, Bool) -> (Vector2, Vector2, Bool)
block (Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) = (blockv . blockh) t
  where
    pr = playerRad
    -- thanks to https://discord.com/users/1276891525451743295
    blockv = if
      | not (px + pr > rx && px - pr < rx + rw) -> id
      | py + pr <= ry && py + ty + pr >= ry
        -> (_1._y .~ ry - pr) . (_2._y .~ 0) . (_3 .~ False)
      | py - pr >= ry + rh && py + ty - pr <= ry + rh
        -> (_1._y .~ ry + rh + pr) . (_2._y .~ 0)
      | otherwise -> id
    blockh = if
      | not (py + pr > ry && py - pr < ry + rh) -> id
      | px + pr <= rx && px + tx + pr >= rx
        -> (_1._x .~ rx - pr) . (_2._x .~ 0)
      | px - pr >= rx + rw && px + tx - pr <= rx + rw
        -> (_1._x .~ rx + rw + pr) . (_2._x .~ 0)
      | otherwise -> id

inputted :: Model -> Model
inputted m@(Select _) = m
inputted m@(Level (LevelData {..}) p) =
  case chlvl (_player^.pos) _lvlnum of
    Just x -> m & _LevelData.player %~ move _lvlnum & _LevelData.lvlnum .~ x
    Nothing -> Select p
  --where f x = ((lvlnum %~ chlvl (x^.player.pos))) x

chlvl :: Vector2 -> Int -> Maybe Int
chlvl p l = if
  | not $ any ((== '2') . fst) at_blocks -> Just l
  | succ l > length tiles' -> Nothing
  | otherwise -> Just (succ l)
  where
    at_blocks = [b | b@(_, x) <- tiles l, fst (rectTo2V x) == rounded p]
rounded :: Vector2 -> Vector2
rounded p = (fromIntegral :: Int -> Float) . (* tilesLen) . (`div` tilesLen) . floor <$> p
