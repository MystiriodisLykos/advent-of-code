import Control.Applicative
import Data.Foldable
import Data.Either
import Data.Maybe
import Data.List.Extra
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified AoC.Common as AoC

import Text.Parsec (parse, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit)
import Text.Parsec.Combinator (many1)

import Debug.Trace

-- Map a value to the values that cannot appear after it
type Rules = IntMap.IntMap (Set.Set Int)
type Order = [Int]

type Input = (Rules, [Order])

ordered :: Rules -> Order -> Maybe Order
ordered rules o = o <$ foldlM go Set.empty o
  where
    go :: Set.Set Int -> Int -> Maybe (Set.Set Int)
    -- go s x | trace (show s ++ " " ++ show x) False = undefined
    go s x = if x `elem` s then
               Nothing
             else
               Just $ s `Set.union` (maybe Set.empty id $ rules IntMap.!? x)

middle :: [a] -> Maybe a
middle xs = xs !? (length xs `div` 2)

part1 :: Input -> AoC.Solution
part1 (rs, os) = sum . catMaybes $ map (go rs) os
  where
    go rs' o = ordered rs' o >>= middle

parseRule :: Parser (Int, Int)
parseRule = do
  a <- many1 digit
  _ <- char '|'
  b <- many1 digit
  return (read a, read b)

parseRules :: Parser Rules
parseRules = do
  rs <- many1 (parseRule <* (newline <|> eof))
  return $ foldl go IntMap.empty rs
  where
    go :: Rules -> (Int, Int) -> Rules
    go rs (i, t) = IntMap.insertWith (Set.union) t (Set.singleton i) rs
    newline = do
      _ <- char '\n'
      return ()

toInput :: String -> Input
toInput s = (rules rs, orders <$> drop 1 os)
  where
    (rs, os) = break ((==) "") $ lines s
    orders = (fmap read) . split ((==) ',')
    rules rs' = fromRight IntMap.empty $ parse parseRules "" (unlines rs')

main :: IO ()
main = do
  input <- toInput <$> AoC.input
  putStrLn $ show $ part1 input
  -- putStrLn $ show $ part2 input

