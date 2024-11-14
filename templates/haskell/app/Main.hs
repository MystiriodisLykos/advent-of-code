{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Arrow
  ( arr,
    first,
    returnA,
    second,
    (&&&),
    (***),
    (>>>),
    (>>^),
    (^>>),
  )
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
    returnA -< text "Test"

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
  let toPic :: DTime -> IO Picture
      toPic = const $ readIORef picRef
      stepWorld :: S.ViewPort -> Float -> DTime -> IO DTime
      stepWorld _ delta timeAcc
        | delta' > 0 = do
            _ <- react handle (delta', Nothing)
            return 0.0
        | otherwise = return (-delta')
        where
          delta' = realToFrac delta - timeAcc -- An action to convert the world to a picture
          -- A function to step the world one iteration. It is passed the period of
          -- time (in seconds) needing to be advanced
  S.simulateIO display color frequency 0 toPic stepWorld
