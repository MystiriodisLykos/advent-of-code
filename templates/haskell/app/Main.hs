{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Arrow as Arrow

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Debug.Trace (trace)
import FRP.Yampa
  ( DTime,
    Event (Event, NoEvent),
    SF,
    accumHold,
    accumHoldBy,
    after,
    catEvents,
    constant,
    dSwitch,
    dropEvents,
    edge,
    event,
    hold,
    iEdge,
    iPre,
    integral,
    isEvent,
    notYet,
    rSwitch,
    react,
    reactInit,
    repeatedly,
    switch,
    tag,
    time,
  )
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture (Pictures),
    black,
    blank,
    blue,
    color,
    green,
    polygon,
    red,
    scale,
    text,
    white,
    yellow,
  )
import qualified Graphics.Gloss.Interface.IO.Simulate as S

simulation :: SF () Picture
simulation =
  proc _ -> do
    p <- integral -< (1 :: Double)
    Arrow.returnA -< text $ show p -- translate t t arrGraph

main :: IO ()
main =
  simulateYampa
    (InWindow "Simulation" (1000, 600) (200, 200))
    white
    30
    simulation

-- | Play the game in a window, updating when the value of the provided
simulateYampa ::
  -- | The display method
  Display ->
  -- | The background color
  Color ->
  -- | The refresh rate, in Hertz
  Int ->
  -- | simulation function
  SF () Picture ->
  IO ()
simulateYampa display color frequency mainSF = do
  picRef <- newIORef blank
  handle <-
    reactInit
      (return ())
      ( \_ updated pic -> do
          when updated (picRef `writeIORef` pic)
          return False
      )
      mainSF
  let toPic :: (DTime, DTime) -> IO Picture
      toPic = const $ readIORef picRef
      stepWorld :: S.ViewPort -> Float -> (DTime, DTime) -> IO (DTime, DTime)
      stepWorld _ delta (timeAcc, timeTotal)
        | delta' > 0 = do
            let delta'' = if timeTotal < (-5) then -delta' else delta'
            _ <- react handle (delta'', Nothing)
            return (0.0, timeTotal')
        | otherwise = return (-delta', timeTotal')
        where
          delta' = realToFrac delta - timeAcc
          timeTotal' = realToFrac (-delta) + timeTotal
  S.simulateIO display color frequency (0, 0) toPic stepWorld
