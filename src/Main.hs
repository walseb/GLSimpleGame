module Main where

import qualified SDL as S
import FRPEngine.Init
import Types
import Level
import FRPEngine.Input.Types
import Control.Concurrent
  ( newMVar,
    readMVar,
    swapMVar,
    threadDelay,
  )
import FRPEngine.Input.Input
import Input
import Data.Maybe
import Render.SDL.Render
import Control.Monad.IO.Class
import qualified SDL.Font as F
import SDL.Image as SI
import Control.Lens
import FRPEngine.Types
import Linear
import FRPEngine.Physics.Collision.GJK

debug = True

type DTime = Double

getResources :: (MonadIO m) => S.Renderer -> m Resources
getResources renderer =
  -- Init fonts
  Resources
    <$> (F.initialize >> F.load fontPath 12)
    <*> load "data/enemy.png" renderer
    <*> load "data/player.png" renderer
  where
    load :: (MonadIO m) => FilePath -> S.Renderer -> m S.Texture
    load path rend = SI.loadTexture rend path
    fontPath = "data/fonts/OpenSans-Regular.ttf"

moveSpeed = 800

updateData :: GameState -> (DTime, [Input]) -> (GameState, Bool)
updateData game (dt, inp) =
    case hasCollided of
      True -> (game, True)
      False -> (move game, False)
  where
    hasCollided = or $ collidesObj (game ^. (physicalState . player)) <$> game ^. (physicalState . enemies)
    -- move a = a
    move = (physicalState . player . obj . pos) %~ (+ (V2 (700 * dt) ((moveKey inp ^. _y) * moveSpeed * dt)))

run a@(renderer, window) resources sense (state, keys) = do
  (dt, events) <- sense False

  let newKeys = updateInput keys (concat (maybeToList events))
      (newState, softDead) = updateData state (dt, newKeys)

  () <- render renderer resources newState
  case softDead || (quitKey keys) of
    True -> pure()
    False -> run a resources sense (newState, newKeys)

main = do
  renderInfo@(renderer, window) <- initSDL "SimpleGame1" S.Windowed
  sense <- getSense
  resources <- getResources renderer

  () <- run renderInfo resources sense (initialGame, keyBinds)

  S.destroyRenderer renderer
  S.destroyWindow window
  where
    -- Delta time
    getDeltaTime :: IO (IO Double, IO Double)
    getDeltaTime = do
      lastTime <- newMVar =<< S.time
      let setDt = do
            -- Get time
            newTime <- S.time
            -- Return the delta time
            -- swapMVar returns the last value, not the new value
            (newTime -) <$> swapMVar lastTime newTime
      let readDt = do
            newTime <- S.time
            (newTime -) <$> readMVar lastTime
      pure (setDt, readDt)
    -- Events
    getEvents = do
      events <- S.pollEvents
      pure $ events
    -- Debug
    oneSecondTimer = do
      delta <- newMVar 0
      let updateTimer dt = do
            deltaVal <- readMVar delta
            swapMVar delta (if deltaVal + dt > 1 then 0 else deltaVal + dt)
      pure updateTimer
    profileFrameTime dt = print ("Framerate: " ++ show (1 / dt))
    getSense = do
      -- Init get delta time
      (setDt, readDt) <- getDeltaTime
      getTimer <- oneSecondTimer
      let sense _canBlock = do
            -- Get the delta time without updating it
            dt' <- readDt
            case dt' < frameCap of
              True -> threadDelay (floor ((frameCap - dt') * 10 ^ 6))
              False -> pure ()
            -- Get the delta time and set it
            dt <- setDt
            -- Get events
            events <- getEvents
            -- Debug frame time
            case debug of
              True -> do
                test <- getTimer dt
                case test == 0 of
                  True -> profileFrameTime dt
                  False -> pure ()
                pure ()
              False -> pure ()
            pure
              -- Prevent delta time from getting higher than frametime for 60 fps
              (min dt frameMaxJump, Just events)
      pure sense
