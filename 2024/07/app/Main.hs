import Control.Applicative
import Data.Function
import Data.Maybe
import qualified AoC.Common as AoC

import Debug.Trace (trace)

type Target = Int
type Values = [Int]
type Input = [(Target, Values)]
type Calibration a = Maybe a

type Check = Target -> Values -> Calibration Target

check1 :: Check -> Check
check1 _ y _ | y <= 0 = Nothing
check1 _ y [x]
  | x == y = Just y
  | otherwise = Nothing
check1 cont y (x:xs)
  | y `mod` x == 0 = y <$ (mult <|> add)
  | otherwise      = y <$ add
  where
    mult = cont (y `div` x) xs
    add = cont (y - x) xs

part1 :: Input -> AoC.Solution
part1 = sum . catMaybes . (fmap $ uncurry (fix check1))

toInput :: String -> Input
toInput = fmap go . lines
  where
    go :: String -> (Target, Values)
    go l = let
      (t, v) = span ((/=) ':') l
      in
      (read t, reverse $ read <$> (drop 1 $ words v))

main :: IO ()
main = do
  input <- toInput <$> AoC.input
  putStrLn $ show $ part1 $ input -- (trace (show input) input)
  -- putStrLn $ show $ part2 input

