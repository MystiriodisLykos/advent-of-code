
import qualified Data.IntMap.Strict as IMap
import Data.List (sort, transpose)
import Debug.Trace (trace)

type Input = [(Int, Int)]
type Counts = (IMap.IntMap Int, IMap.IntMap Int)
type Solution = Int

toInput :: String -> Input
toInput = map (pair . words) . lines
  where
    pair [a, b] = (read a, read b)
    pair otherwise = error "Each line of the input should have exactly 2 values"

count :: Input -> Counts
count = go (IMap.empty, IMap.empty)
  where
    go cs []                 = cs
    go (as, bs) ((a, b): xs) = go (add a as, add b bs) xs
    add k = IMap.insertWith (+) k 1

-- Reference implementation using a standard sort and zip.
part1Ref :: Input -> Int
part1Ref = sum . map abs . join . map sort . transpose . map unpair
  where
    join [a, b] = zipWith (-) a b
    unpair (a, b) = [a, b]

-- Number of pairs in progress.
type Pairs = Int
-- Location ID
type ID = Int
-- Differences between the number of occurances of an ID in the first list
-- from the second list
type Occurance = Int

-- This solution spawned out of trying to exploit the identity
-- `|a-b| = |b-a|`
-- Which leads to the implication
-- `|a-b| + |b-c| -> c-a` where `a <= b <= c`.
-- This means that each instance of a value `b` occuring in both list adds
-- nothing to the solution and can be ignored.
-- The `cs` accounts for this by finding the difference between occurances of
-- both lists.
-- The `join` function does the zip of the reference implementation
-- but it takes advantage of the fact the occurance list is already sorted.
-- Since generating it is a radix sort.
part1 :: Counts -> Int
part1 (as, bs) = abs . fst $ IMap.foldlWithKey join (0, 0) cs
  where
    -- join the current solution, and in progress pairs with a given
    -- location id and number of occurances of the id
    join :: (Solution, Pairs) -> ID -> Occurance -> (Solution, Pairs)
    -- join (r, p) v a | trace (joinShow r p v a) False = undefined
    join r v a      | a == 0 = r
    join (r, p) v a | p == 0 = join' r p v a   1
    join (r, p) v a | p >  0 = join' r p v a $ signum a
    join (r, p) v a | p <  0 = join' r p v a $ signum (-a)
    join' r p v a d = join (r-(v*d), p+(signum a)) v (a-(signum a))
    joinShow r p v a =  "r: " ++ show r
                     ++ " p: " ++ show p
                     ++ ", v: " ++ show v
                     ++ ", a: " ++ show a
    cs = IMap.unionWith (+) as $ ((-) 0) <$> bs

main :: IO ()
main = do
  input <- toInput <$> readFile "input"
  putStrLn $ show $ part1Ref input
  putStrLn $ show $ part1 $ count input
