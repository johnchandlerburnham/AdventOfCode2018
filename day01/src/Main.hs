module Main where

import System.IO
import qualified Data.Set as Set

readInt :: String -> Integer
readInt ('+':n) = read n
readInt n = read n

part1 :: String -> Integer
part1 input = sum $ readInt <$> words input

part2 :: String -> Integer
part2 input = go 0 ds (Set.singleton 0)
  where
    ds = readInt <$> words input
    go x (n:ns) set = let x' = n + x in
      if x' `Set.member` set
      then x'
      else go x' ns (x' `Set.insert` set)
    go x [] set = go x ds set


main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()


