{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Lens
  ( Field1 (_1),
    Field2 (_2),
    Field3 (_3),
    (%~),
    (&),
    (.~),
    (^.), (^?),
  )
import Control.Monad (forM, guard, when)
import Data.Foldable (for_)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, fromJust)
import Data_ (playerRad, tiles, tiles', tilesLen, restart)
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Raylib.Core.Textures as Texture ( drawTextureV )
import Raylib.Types.Core (Rectangle (..), Vector2)
import Raylib.Types.Core.Camera (Camera2D (Camera2D))
import Raylib.Types.Core.Textures as Textures (Texture)
import Raylib.Util.Colors (white, black)
import Raylib.Util.GUI (guiButton)
import Raylib.Util.Lenses (_rectangle'x, _rectangle'y)
import Types
import Utils (rectTo2V, _LevelData, liftMaybe, appMaybe)
import Raylib.Core.Text (drawText, measureText)
import Data.List ((!?))

-- draw

-- thanks to https://discord.com/users/1276891525451743295
drawtiles :: [(Char, Rectangle)] -> M.Map Char Texture -> IO ()
drawtiles t i =
  for_
    t
    ( \(id', x) ->
        for_
          (M.lookup id' i)
          ( \i' ->
              drawTextureV i' (V2 (x ^. _rectangle'x) (x ^. _rectangle'y)) white
          )
    )

drawplayer :: Vector2 -> Texture -> IO ()
drawplayer v i = drawTextureV i (v - pure playerRad) white

drawgui :: (Float, Float) -> IO (Maybe Int)
drawgui (w, h) = do
  let m = length tiles'
  let ah = h / fromIntegral m
  clicks <- forM [0 .. m - 1] \x -> do
    let xf = fromIntegral x
    clicked <-
      guiButton
        Rectangle
          { rectangle'x = 0,
            rectangle'y = xf * ah,
            rectangle'width = w,
            rectangle'height = ah
          }
        (Just (show (succ x)))
    return if clicked then Just x else Nothing
  return $ listToMaybe $ catMaybes clicks

drawmain :: (Int, Int) -> IO Bool
drawmain (w, h) = do
  tw <- measureText "game" 240
  drawText "game" ((w `div` 2) - (tw `div` 2)) (h `div` 6) 240 black
  guiButton Rectangle {
    rectangle'x = 0,
    rectangle'y = fromIntegral (h `div` 2),
    rectangle'width = fromIntegral w,
    rectangle'height = fromIntegral (h `div` 2)
  } (Just "start")

-- misc

center :: Vector2 -> Int -> Int -> Camera2D
center v c d = Camera2D ((/ 2) . fromIntegral <$> V2 c d) v 0 1

move :: Int -> Player -> Player
move l m@Player {_pos = p, _uvel = v, _tvel = t, _air = a} =
  m {_pos = p' + t', _tvel = t', _air = a'}
  where
    t1 = (v + V2 0 1 +) $ t & _x %~ (* 0.7)
    (p', t', a') = foldr block (p, t1, a) [y | (x, y) <- fromMaybe [] (tiles l), x == '1']

block :: Rectangle -> (Vector2, Vector2, Bool) -> (Vector2, Vector2, Bool)
block (Rectangle rx ry rw rh) t@(V2 px py, V2 tx ty, _) = (blockv . blockh) t
  where
    pr = playerRad
    -- thanks to https://discord.com/users/1276891525451743295
    blockv =
      if
        | not (px + pr > rx && px - pr < rx + rw) -> id
        | py + pr <= ry && py + ty + pr >= ry ->
            (_1 . _y .~ ry - pr) . (_2 . _y .~ 0) . (_3 .~ False)
        | py - pr >= ry + rh && py + ty - pr <= ry + rh ->
            (_1 . _y .~ ry + rh + pr) . (_2 . _y .~ 0)
        | otherwise -> id
    blockh =
      if
        | not (py + pr > ry && py - pr < ry + rh) -> id
        | px + pr <= rx && px + tx + pr >= rx ->
            (_1 . _x .~ rx - pr) . (_2 . _x .~ 0)
        | px - pr >= rx + rw && px + tx - pr <= rx + rw ->
            (_1 . _x .~ rx + rw + pr) . (_2 . _x .~ 0)
        | otherwise -> id

inputted :: Model -> Model
inputted m@(Level (LevelData {..}) p) =
  case chlvl (_player ^. pos) _lvlnum of
    Just x -> m & _LevelData %~ (player %~ move _lvlnum) . (lvlnum .~ x)
    Nothing -> Main p
inputted m = m

-- where f x = ((lvlnum %~ chlvl (x^.player.pos))) x

chlvl :: Vector2 -> Int -> Maybe Int
chlvl p l = if
  | not $ any ((== '2') . fst) at_blocks -> Just l
  | succ l > length tiles' -> Nothing
  | otherwise -> Just (succ l)
  where
    at_blocks = [b | b@(_, x) <- fromMaybe [] (tiles l), fst (rectTo2V x) == rounded p]

tolevel :: Int -> Persistent -> Model
tolevel l = Level $ restart & lvlnum .~ l & player . pos %~ appMaybe d
  where
    c = listToMaybe do
      ca <- liftMaybe (tiles l)
      (t, v) <- ca
      guard (t == '3')
      return v
    d = (\c' -> fst (rectTo2V c') + pure (tilesLen / 2)) <$> c


rounded :: Vector2 -> Vector2
rounded p = (fromIntegral :: Int -> Float) . (* tilesLen) . (`div` tilesLen) . floor <$> p
