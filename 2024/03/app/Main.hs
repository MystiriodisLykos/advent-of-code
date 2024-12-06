{-# LANGUAGE ApplicativeDo #-}
import Data.Maybe
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)
import System.Environment (getArgs)

type Mul = (Int, Int)
type Solution = Int
type Input = String

mul :: RE Char Mul
mul = do
  _ <- string "mul("
  a :: Int <- decimal
  _ <- string ","
  b :: Int <- decimal
  _ <- string ")"
  return (a, b)

findAll :: RE a b -> RE a [b]
findAll re = catMaybes <$> many ((Just <$> re) <|> (Nothing <$ anySym))

part1 :: Input -> Solution
part1 s = maybe 0 go (match (findAll mul) s)
  where
    go = sum . map (uncurry (*))

main :: IO ()
main = do
  file <- headDef "input" <$> getArgs
  input <- readFile file
  putStrLn $ show $ part1 input

headDef :: a -> [a] -> a
headDef a [] = a
headDef _ (x:_) = x
