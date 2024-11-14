{-# LANGUAGE Arrows, KindSignatures, MultiParamTypeClasses, ConstraintKinds, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

import Control.Arrow                      ( returnA, (>>>), (^>>), (>>^), (&&&), (***), arr, first, second )
import FRP.Yampa                          ( SF, Event (Event, NoEvent), DTime, reactInit, react, time
                                          , tag, catEvents, accumHold, after, notYet, isEvent
                                          , accumHoldBy, dSwitch, constant, iPre, event
                                          , edge, integral, hold, switch, rSwitch
                                          , iEdge, repeatedly, switch, dropEvents )
import Graphics.Gloss                     ( Display (InWindow)
                                          , Picture (Pictures)
                                          , Color, blank
                                          , polygon, scale
                                          , white, black
                                          , green, blue
                                          , red, yellow
                                          , color, text
                                          )

import qualified Graphics.Gloss.Interface.IO.Simulate as S

import Debug.Trace (trace)

import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Control.Monad                    (when)


simulation :: SF () Picture
simulation = proc _ -> do
  returnA -< text "Test"

main :: IO ()
main = simulateYampa (InWindow "Simulation" (1000, 600) (200, 200)) white 30 simulation

-- | Play the game in a window, updating when the value of the provided
simulateYampa :: Display                 -- ^ The display method
              -> Color                   -- ^ The background color
              -> Int                     -- ^ The refresh rate, in Hertz
              -> SF () Picture           -- ^ simulation function
              -> IO ()
simulateYampa display color frequency mainSF = do
  picRef <- newIORef blank

  handle <- reactInit
              (return ())
              (\_ updated pic -> do when updated (picRef `writeIORef` pic)
                                    return False
              )
              mainSF

  let -- An action to convert the world to a picture
      toPic :: DTime -> IO Picture
      toPic = const $ readIORef picRef

      -- A function to step the world one iteration. It is passed the period of
      -- time (in seconds) needing to be advanced
      stepWorld :: S.ViewPort -> Float -> DTime -> IO DTime
      stepWorld _ delta timeAcc
          | delta' > 0 = do
              _ <- react handle (delta', Nothing)
              return 0.0
          | otherwise  = return (-delta')
        where
          delta' = realToFrac delta - timeAcc

  S.simulateIO display color frequency 0 toPic stepWorld
