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
import GHC.Float (double2Float)
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
    pause,
    delay
  )
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture (Pictures),
    Point,
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

import Control.Category (Category(..))

import Data.Foldable (toList)
import Data.Sequence (fromList, empty, index, (|>), (><))
import qualified Data.Sequence as Seq
import Data.VectorSpace (VectorSpace)
import Debug.Trace (trace)

import Prelude hiding (id, (.))

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

newtype SFP a b = SFP {unSFP :: (SF (Event (a, Picture)) (Picture, Event (b, Picture)))}

data SFP' a b = SFP' {
  start :: Point,
  end :: Point,
  sf :: SF a (b, Event (SF () Picture)),
  sfp :: SF (Event (SF () Picture)) Picture,
  inp :: SF a Picture,
  outp :: SF b Picture
  }

idSFP' :: (Show a) => Point -> Point -> SFP' a a
idSFP' s@(sx, sy) e@(ex, ey) = SFP' s e (arr id &&& never) (constant $ line s e) (arr $ (translate sx (sy+10)) . text . show) (arr $ (translate ex (ey+10)) . text . show)

compSFP' :: (Show a, Show b, Show c) => SFP' b c -> SFP' a b -> SFP' a c
compSFP' f g = SFP' s e sf inp outp
  where
    s = start f
    e = end g
    sf' :: SF a (c, (Event (SF () Picture), Event (SF () Picture)))
    sf' = proc a -> do
      (b, p1) <- sf g -< a
      (c, p2) <- sf f -< b
      returnA -< (c, (p1, p2))
    sf = sf' >>^ fst
    sfp = sf' >>> proc (_, (p1, p2)) -> do
      r1 <- sfp g -< p1
      r2 <- sfp f -< p2
      returnA -< pictures [r1 r2]
    inp = 

-- data YampaAni = ID a a
--               | Comp a b c
--               | Arr a b

type DropN = Int
type ConcatSFs a b = [SF a b]

eventComp :: Event (a -> a) -> Event (a -> a) -> Event (a -> a)
eventComp (Event f) (Event g) = Event $ f . g
eventComp (Event f) NoEvent   = Event f
eventComp NoEvent   (Event g) = Event g
eventComp NoEvent   NoEvent   = NoEvent

rpQueueB :: SF (Event (ConcatSFs () b), Event DropN) [b]
rpQueueB = (constant ()) &&& ins >>> rpSwitchB empty >>^ toList
  where
    ins = arr $ uncurry go
    go sfs ds = eventComp (Seq.drop <$> ds) (adds <$> sfs)
    adds as = (flip (><)) $ fromList as

idAni :: Double -> Double -> SFP a a
idAni v d = SFP $ proc e -> do
  rec
    as  <- accumHold empty -< eventComp (de `tag` Seq.drop 1) $ (concat) <$> e
    de  <- delay (d/v) NoEvent -< e `tag` 1
    ps  <- rpQueueB        -< ((element . snd) <$> e, de)
  returnA -< (pictures $ ps ++ [arrL $ double2Float d], de `tag` (index as 0))
  where
    concat a as = as |> a
    element :: Picture -> [SF () (Picture)]
    element p = [constant v >>> integral >>^ double2Float >>^ (\x -> (translate x 0 p))]

compAni :: SFP b c -> SFP a b -> SFP a c
compAni (SFP f) (SFP g) = SFP $ proc e -> do
  (p, e') <- g -< e
  (p', e'') <- f -< e'
  returnA -< (pictures [p, translate 200 0 p'], e'')

instance Control.Category.Category SFP where
  id = idAni 50 200
  (.) = compAni

-- arr' :: (a -> b) -> (b -> Picture) -> SFP a b

arrGraph :: Picture
arrGraph = pictures [ circle 20
                    , rectangleWire 80 70
                    , line [(-80, 0), (-20, 0)]
                    , line [(20, 0), (80, 0)]
                    , polygon [(-80, 0), (-100, -10), (-100, 10)]
                    , polygon [(80, 10), (80, -10), ( 100, 0)]
                    ]

arrL :: Float -> Picture
arrL d = pictures [ line [(0, 0), (d, 0)]
                  , polygon [(0, 0), (-20, 10), (-20, -10)]
                  , polygon [(d, 0), (d-20, 10), (d-20, -10)]
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
simulation = repeatedly 2 ((), circle 20) >>> (unSFP $ id . id) >>^ fst

-- simulation :: SF () Picture
-- simulation = (pause 0 sfC time) >>^ text . show
--   where
--     sfC = proc _ -> do
--       t <- time -< ()
--       returnA -< t < 5 || (t > 10 && t < 15)

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
