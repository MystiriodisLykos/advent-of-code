
import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad.Extra (fold1M)
import qualified Data.IntMap.Strict as IMap
import Data.Functor (($>))
import Data.Functor.Product (Product(Pair))
import Data.Functor.Classes (Show1)
import Data.List (sort, transpose)
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(Sum), First(First))
import System.Environment (getArgs)
import Debug.Trace (trace)

type Input = [[Int]]
type Solution = Int

toInput :: String -> Input
toInput = map (map read . words) . lines

headDef :: a -> [a] -> a
headDef a [] = a
headDef _ (x:_) = x

part1 :: Input -> Solution
part1 = length . catMaybes . map (join' . fold1M safe)
  where
    safeJump :: Int -> Int -> Maybe Int
    safeJump a b | a == b = Nothing
                 | abs (a-b) > 3 = Nothing
                 | otherwise = Just b
    asc :: Int -> Int -> Maybe Int
    asc a b | a < b = Just b
            | otherwise = Nothing
    des :: Int -> Int -> Maybe Int
    des a b | a > b = Just b
            | otherwise = Nothing
    join (j, a, d) = j >> (a <|> d)
    -- join' r | trace (show r) False = undefined
    join' (Pair j (Pair a d)) = join (j, a, d)
    safe :: Int -> Int -> Product Maybe (Product Maybe Maybe) Int
    -- safe a b | trace (show a ++ " " ++ show b) False = undefined
    safe a b =
      let
        jump = safeJump a b
        asc' = asc a b
        des' = des a b
      in
        Pair (join (jump, asc', des')) $ Pair (asc' >> jump) (des' >> jump)

main :: IO ()
main = do
  file <- headDef "input" <$> getArgs
  input <- toInput <$> readFile file
  putStrLn $ show $ part1 input
