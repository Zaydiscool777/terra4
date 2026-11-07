module Utils where

import Types
import Data_

import Raylib.Types.Core
import Control.Monad.State
import Raylib.Core
import Control.Monad
import Data.Bifunctor (first)
import Linear
import Control.Lens

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht
loopUntil :: Monad m => (a -> Bool) -> StateT s m a -> s -> m ([a], a)
loopUntil p m s = do
  (x, s') <- runStateT m s
  if p x then return ([], x) else first (x :) <$> loopUntil p m s'

-- from: split
-- chunksOf :: Int -> [e] -> [[e]]
-- chunksOf i ls = map (take i) (splitter ls (:) [])
--   where
--     splitter :: [e] -> ([e] -> a -> a) -> a -> a
--     splitter [] _ n = n
--     splitter l c n = l `c` splitter (drop i l) c n

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s [a]
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ([a], a)
untilM p m = loopUntil p (get >>= lift . m)

untilM' :: Monad m => (a -> Bool) -> m a -> m ([a], a)
untilM' p m = loopUntil p (lift m) ()

persistent :: Lens' Model Persistent
persistent = lens get' set''
  where
    get' (Level _ p) = p
    get' (Select p)  = p
    set'' (Level ld _) p = Level ld p
    set'' (Select _)   p = Select p

_LevelData :: Traversal' Model LevelData
_LevelData = _Level._1

-- lesser utils

tilClose :: StateT Model IO () -> StateT Model IO ()
tilClose x = void $ untilM' id (x >> liftIO windowShouldClose)

-- getKeysPressed :: IO [KeyboardKey]
-- getKeysPressed = untilM' (== KeyNull) getKeyPressed

getKeysDown :: IO [KeyboardKey]
getKeysDown = filterM isKeyDown (toEnum <$> keysint)

rectTo2V :: Rectangle -> (Vector2, Vector2)
rectTo2V (Rectangle x y w h) = (V2 x y, V2 (x + w) (y + h))
