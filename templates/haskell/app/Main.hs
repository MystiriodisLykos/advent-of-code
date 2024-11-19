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
    integral,
    kSwitch,
    rpSwitchB,
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
    circle,
    rectangleWire,
    pictures,
    line,
    translate
  )
import qualified Graphics.Gloss.Interface.IO.Simulate as S

import qualified Control.Category (Category(..))

import Data.Foldable (toList)
import Data.Sequence (fromList, empty, (|>), (><))
import qualified Data.Sequence as Seq
import Data.VectorSpace (VectorSpace)

-- class Graphable g where
--   data f a :: * -> *
--   graph :: SF (Event (f a)) Picture
--   ins :: f Point
--   outs :: f Point

-- newtype Arr a b c = Arr (a b c)

-- arrGraph :: (Show a) => SF (Event a) Picture
-- arrGraph =
--   proc (Identity e) -> do
--     Arrow.returnA -< arrGraph'
--   where
--     arrGraph' = 

-- instance Graphable (Arr SF a b) where
--   data f a :: * -> *
--   graph :: SF (Event (f a)) Picture
--   ins :: f Point

-- data SFP a b = SFP {
--   core :: SF (Event a) (Event b),
--   sf_ani :: SF (Event (SF () Picture)) Picture,
--   in_ani :: SF (Event Picture) (Picture, Event ()),
--   out_ani :: SF (Event b) (Picture, Event Picture)
--   -- picture :: SF (Event Picture, Event b) (Picture, Event Picture),
--   }

-- animate :: SFP a b -> SF (Event (a, Picture)) (Picture, Event b)
-- animate SFP(c b i o) = undefined
--   where
--     i_ani :: SF (Event (a, Picture)) (Picture, Event b)

-- drawId :: SF a Picture
-- drawId = showCircle

-- instance Control.Category.Category SFP where
--   id :: SFP a a
--   id = SFP (arr id) drawId undefined undefined
--   (.) :: SFP b c -> SFP a b -> SFP a c
--   (.) = undefined

-- newtype SFP a b = SFP (SF (Event (a, Picture)) (Picture, Event (b, Picture)))

integrals :: (Fractional s, VectorSpace a s) => SF (Event [a], Event Int) [a]
integrals = (constant ()) &&& ins >>> rpSwitchB empty >>^ toList
  where
    ins = arr $ uncurry go
      where
        go (Event as) (Event ds) = Event $ (Seq.drop ds) . (adds as)
        go (Event as) NoEvent    = Event $ adds as
        go NoEvent    (Event ds) = Event $ Seq.drop ds
        go NoEvent    NoEvent    = NoEvent
        adds as = (flip (><)) . (fmap (\v -> constant v >>> integral)) $ fromList as

idAni :: SF (Event (a, Picture)) (Picture)
idAni = proc e -> do
  rec
    as <- accumHold empty -< (concat . fst) <$> e
    ps <- accumHold empty -< (concat . snd) <$> e
    psp <- integrals -< (e `tag` [50], NoEvent)
  returnA -< pictures $ zipWith (\x p -> translate x 0 p) psp $ toList ps
  where
    concat a as = as |> a

-- instance Control.Category.Category SFP where
--   id = SFP 

-- arr' :: (a -> b) -> (b -> Picture) -> SFP a b

arrGraph :: Picture
arrGraph = pictures [ circle 20
                    , rectangleWire 80 70
                    , line [(-80, 0), (-20, 0)]
                    , line [(20, 0), (80, 0)]
                    , polygon [(-80, 0), (-100, -10), (-100, 10)]
                    , polygon [(80, 10), (80, -10), ( 100, 0)]
                    ]

showTime :: SF a Picture
showTime =
  proc _ -> do
    p <- integral -< (1 :: Double)
    Arrow.returnA -< text $ show p

showCircle :: SF a Picture
showCircle = proc _ -> do
  p <- integral -< 100
  Arrow.returnA -< translate p 0 $ circle 20

kSwitchR :: SF a b -> SF (a, b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitchR sf e f = kSwitch sf e cont
  where
    cont sf' c = rHctiwsK sf' $ f sf' c
    rHctiwsK sf' sf'' = kSwitch sf'' e' (\_ _ -> kSwitchR sf' e f)
    e' = ((time >>^ ((>) 0)) >>> edge)

-- simulation :: SF () Picture
-- simulation = (kSwitchR showTime ((time >>^ ((<) 2)) >>> edge) (\_ _ -> showCircle)) &&&
--   (showTime >>^ translate 0 100) >>^ (\(a, b) -> pictures [a, b])

simulation :: SF () Picture
simulation = repeatedly 2 ((), circle 20) >>> idAni

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
            -- let delta'' = if 10 > timeTotal && timeTotal > 5 then -delta' else delta'
            _ <- react handle (delta', Nothing)
            return (0.0, timeTotal')
        | otherwise = return (-delta', timeTotal')
        where
          delta' = realToFrac delta - timeAcc
          timeTotal' = realToFrac delta + timeTotal
  S.simulateIO display color frequency (0, 0) toPic stepWorld
