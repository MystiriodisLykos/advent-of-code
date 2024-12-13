import Control.Applicative
import Data.Maybe
import Data.List
import qualified AoC.Common as AoC

type Input = [String]

mapt :: ([a] -> b) -> [a] -> [b]
mapt f = fmap f . tails

bool2Int :: Bool -> Int
bool2Int True = 1
bool2Int False = 0

isXmas :: String -> Bool
isXmas "XMAS" = True
isXmas "SAMX" = True
isXmas _      = False

isXmasR :: [String] -> Bool
isXmasR ((x:m:a:s : _) : _) = isXmas $ x:m:a:[s]
isXmasR _ = False

isXmasD1 :: [String] -> Bool
isXmasD1 ( (x: _)
         : (_: m: _)
         : (_: _: a: _)
         : (_: _: _: s: _): _) = isXmas $ x:m:a:[s]
isXmasD1 _ = False

isXmasD2 :: [String] -> Bool
isXmasD2 ( (_: _: _: s: _)
         : (_: _: a: _)
         : (_: m: _)
         : (x: _): _) = isXmas $ x:m:a:[s]
isXmasD2 _ = False

isXmasC :: [String] -> Bool
isXmasC ( (x:_)
        : (m:_)
        : (a:_)
        : (s:_): _) = isXmas $ x:m:a:[s]
isXmasC _ = False

countXmas :: [String] -> Int
countXmas g = sum $ bool2Int <$> (ZipList [ isXmasR
                                          , isXmasD1
                                          , isXmasD2
                                          , isXmasC] <*> ZipList (repeat g))

part1 :: Input -> AoC.Solution
part1 = sum . mapt (sum . mapt (countXmas) . transpose)

main :: IO ()
main = do
  input <- transpose <$> reverse . lines <$> AoC.input
  putStrLn $ show $ part1 input
  -- putStrLn $ show $ part2 input

