{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Elm (init', tick, tilClose) where

import Control.Lens ((%~), (&), (.~), (^.), (^?))
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
  )
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data_ (playerRad, tiles, tilesLen, restart)
import Lib
  ( center,
    drawgui,
    drawplayer,
    drawtiles,
    inputted,
    drawmain,
  )
import Linear (V2 (V2))
import Raylib.Core
  ( beginDrawing,
    beginMode2D,
    clearBackground,
    endDrawing,
    endMode2D,
    getScreenHeight,
    getScreenWidth,
  )
import Raylib.Types.Core
  ( KeyboardKey (KeyDown, KeyLeft, KeyQ, KeyR, KeyRight, KeyUp, KeyX),
  )
import Raylib.Util.Colors (lightGray, rayWhite, blue)
import Types

import Utils (getKeysDown, tilClose, _LevelData, persistent)
import Control.Monad ((>=>))
import Raylib.Core.Textures
    ( imageResize, loadImage, loadTextureFromImage )
-- import Raylib.Types (Texture)

init' :: IO Model
init' = do
  let squaretexture l = loadImage >=> (\p -> imageResize p l l) >=> loadTextureFromImage
  img0 <- squaretexture (playerRad * 2) "./assets/ball.png"
  img1 <- squaretexture tilesLen "./assets/wall.png"
  img2 <- squaretexture tilesLen "./assets/portal.png"
  return $
    Main
      Persistent
        { _ptex = img0,
          _tiletex =
            M.fromList
              [ ('1', img1),
                ('2', img2)
              ]
        }

tick :: StateT Model IO ()
tick = get >>= liftIO . (sub >=> view') >>= put

sub :: Model -> IO Model
sub i = do
  x <- getKeysDown
  let i1 = upd i AStart
  let i2 = foldr (flip upd . AKey) i1 x -- thanks to https://discord.com/users/1276891525451743295
  return $ upd i2 AEnd

upd :: Model -> Action -> Model
upd m AStart = m & (_LevelData . player . uvel .~ pure 0)
upd (Level _ p) (AKey KeyQ) = Select p
upd m (AKey KeyUp) =
  m
    & _LevelData . player . air .~ True
    & _LevelData . player . uvel %~ (+ V2 0 if fromMaybe False $ m ^? _LevelData . player . air then 0 else -22)
upd m (AKey KeyDown) = m & _LevelData . player . uvel %~ (+ V2 0 10)
upd m (AKey KeyLeft) = m & _LevelData . player . uvel %~ (- V2 10 0)
upd m (AKey KeyRight) = m & _LevelData . player . uvel %~ (+ V2 10 0)
upd m (AKey KeyR) = m & _LevelData . player .~ _player restart
upd m (AKey KeyX) = Main (m ^. persistent)
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
      beginMode2D (center (_player ^. pos) w h)
      case tiles _lvlnum of
        Nothing -> return $ Main p
        Just x -> do
          drawtiles x (p ^. tiletex)
          drawplayer (_player ^. pos) (p ^. ptex)
          return m
    Select p -> do
      clearBackground lightGray
      x <- drawgui (fromIntegral w, fromIntegral h)
      return case x of
        Just x' -> Level (restart {_lvlnum = x'}) p
        Nothing -> m
    Main p -> do
      clearBackground blue
      x <- drawmain (w, h)
      return if x then Level restart p else m
  endMode2D
  endDrawing -- presents frame to user
  return m'
