{-# LANGUAGE LexicalNegation, BlockArguments, RecordWildCards #-}
module Elm (init', tick, tilClose) where

import Lib
import Types
import Utils
import Data'

import Raylib.Core
import Raylib.Util.Colors
import Raylib.Types.Core

import Control.Monad.State
import qualified Data.Map as M
import Control.Lens
import Linear
import Data.Maybe

init' :: IO Model
init' = do
  img0 <- squaretexture (playerRad * 2) "./ball.png"
  img1 <- squaretexture tilesLen "./wall.png"
  img2 <- squaretexture tilesLen "./portal.png"
  return $ Level $ LevelData {
    _player = Player {
      _pos = pure 200,
      _uvel = pure 0,
      _tvel = pure 0,
      _air = True
    },
    _image = (
      img0,
      M.fromList [
        ('1', img1),
        ('2', img2)
      ]),
    _lvlnum = 0
  }

tick :: StateT Model IO ()
tick = do
  model <- get
  new <- liftIO (sub model)
  liftIO (view' model)
  put new

sub :: Model -> IO Model
sub i = do
  x <- getKeysDown
  let i1 = upd i AStart
  let i2 = foldr (flip upd . AKey) i1 x -- thanks to https://discord.com/users/1276891525451743295
  return $ upd i2 AEnd

upd :: Model -> Action -> Model
upd m AStart = m & _Level.player.uvel .~ pure 0
upd m (AKey KeyUp) = m
  & _Level.player.air .~ True
  & _Level.player.uvel %~ (+ V2 0 if fromMaybe False $ m^?_Level.player.air then 0 else -22)
upd m (AKey KeyDown) = m & _Level.player.uvel %~ (+ V2 0 10)
upd m (AKey KeyLeft) = m & _Level.player.uvel %~ (- V2 10 0)
upd m (AKey KeyRight) = m & _Level.player.uvel %~ (+ V2 10 0)
upd m (AKey KeyR) = m & _Level.player .~
  Player {
    _pos = pure 201,
    _uvel = pure 0,
    _tvel = pure 0,
    _air = True
  }
upd m AEnd = inputted m
upd m _ = m

view' :: Model -> IO ()
view' m = do
  beginDrawing
  clearBackground rayWhite
  w <- getScreenWidth
  h <- getScreenHeight
  case m of
    Level (LevelData {..}) -> do
      beginMode2D (center (_player^.pos) w h)
      drawtiles (tiles _lvlnum) (_image^._2)
      drawplayer (_player^.pos) (_image^._1)
    Select -> do
      return () -- to do
  endMode2D
  endDrawing -- presents frame to user
