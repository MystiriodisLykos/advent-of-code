module AoC.Common
  (
    Solution
  , headDef
  , input'
  , input
  ) where

import System.Environment (getArgs)

type Solution = Int

headDef :: a -> [a] -> a
headDef a [] = a
headDef _ (x:_) = x

input' :: String -> IO String
input' d = (headDef d) <$> getArgs >>= readFile

input :: IO String
input = input' "input"
