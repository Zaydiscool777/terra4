{-# LANGUAGE LexicalNegation, BlockArguments, RecordWildCards #-}
module Elm (init', tick, tilClose) where

import Lib
import Types
import Utils
import Data_

import Raylib.Core
import Raylib.Util.Colors

import Raylib.Types.Core

import Linear
import Control.Lens
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M

init' :: IO Model
init' = do
  img0 <- squaretexture (playerRad * 2) "./ball.png"
  img1 <- squaretexture tilesLen "./wall.png"
  img2 <- squaretexture tilesLen "./portal.png"
  return $ Level LevelData {
    _player = Player {
      _pos = pure 200,
      _uvel = pure 0,
      _tvel = pure 0,
      _air = True
    },
    _lvlnum = 0
  } Persistent {
    _ptex = img0,
    _tiletex = M.fromList [
      ('1', img1),
      ('2', img2)
    ]
  }

tick :: StateT Model IO ()
tick = get >>= liftIO . sub >>= liftIO . view' >>= put

sub :: Model -> IO Model
sub i = do
  x <- getKeysDown
  let i1 = upd i AStart
  let i2 = foldr (flip upd . AKey) i1 x -- thanks to https://discord.com/users/1276891525451743295
  return $ upd i2 AEnd

upd :: Model -> Action -> Model
upd m AStart = m & (_Level._1.player.uvel .~ pure 0)
upd (Level _ p) (AKey KeyQ) = Select p
upd m (AKey KeyUp) = m
  & _Level._1.player.air .~ True
  & _Level._1.player.uvel %~ (+ V2 0 if fromMaybe False $ m^?_Level._1.player.air then 0 else -22)
upd m (AKey KeyDown) = m & _Level._1.player.uvel %~ (+ V2 0 10)
upd m (AKey KeyLeft) = m & _Level._1.player.uvel %~ (- V2 10 0)
upd m (AKey KeyRight) = m & _Level._1.player.uvel %~ (+ V2 10 0)
upd m (AKey KeyR) = m & _Level._1.player .~
  Player {
    _pos = pure 201,
    _uvel = pure 0,
    _tvel = pure 0,
    _air = True
  }
upd m AEnd = inputted m
upd m _ = m

view' :: Model -> IO Model
view' m = do
  beginDrawing
  w <- getScreenWidth
  h <- getScreenHeight
  m' <- case m of
    Level (LevelData {..}) p -> do
      clearBackground rayWhite
      beginMode2D (center (_player^.pos) w h)
      drawtiles (tiles _lvlnum) (p^.tiletex)
      drawplayer (_player^.pos) (p^.ptex)
      return m
    Select p -> do
      clearBackground lightGray
      x <- drawgui (fromIntegral w, fromIntegral h)
      return case x of
        Just x' ->
          Level LevelData {
            _player = Player {
              _pos = pure 200,
              _uvel = pure 0,
              _tvel = pure 0,
              _air = True
            },
            _lvlnum = x'
          } p
        Nothing -> m
  endMode2D
  endDrawing -- presents frame to user
  return m'
