{-# LANGUAGE ApplicativeDo #-}
import Data.Maybe
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)
import System.Environment (getArgs)

type Mul = (Int, Int)
type Solution = Int
type Input = String

data Statement = SMul Int Int
               | Do
               | Dont
               deriving (Show, Eq)

unMul :: Statement -> (Int, Int)
unMul (SMul a b) = (a, b)
unMul _ = (0, 0)

mul :: RE Char Mul
mul = do
  _ <- string "mul("
  a :: Int <- decimal
  _ <- string ","
  b :: Int <- decimal
  _ <- string ")"
  return (a, b)

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

smul :: RE Char Statement
smul = uncurry SMul <$> mul

do' :: RE Char Statement
do' = string "do()" $> Do

don't :: RE Char Statement
don't = string "don't()" $> Dont

findAll :: RE a b -> RE a [b]
findAll re = catMaybes <$> many ((Just <$> re) <|> (Nothing <$ anySym))

part1 :: Input -> Solution
part1 s = maybe 0 go (match (findAll mul) s)
  where
    go = sum . map (uncurry (*))

part2 :: Input -> Solution
part2 s = maybe 0 go (match (findAll $ smul <|> do' <|> don't) s)
  where
    go = sum . map (uncurry (*) . unMul) . goDo
    goDo [] = []
    goDo statements = let
      (ms, xs) = span ((/=) Dont) statements
      in ms ++ goDon't xs
    goDon't statements = let
      (_, xs) = span ((/=) Do) statements
      in goDo xs

main :: IO ()
main = do
  file <- headDef "input" <$> getArgs
  input <- readFile file
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input

headDef :: a -> [a] -> a
headDef a [] = a
headDef _ (x:_) = x
