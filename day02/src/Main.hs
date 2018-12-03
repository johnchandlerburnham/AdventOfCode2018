module Main where

import           Data.Foldable
import           Data.List
import qualified Data.Map.Lazy as Map
import           System.IO


letterCounts :: String -> Map.Map Char Integer
letterCounts = foldl' (\ m k -> Map.insertWith (\n o -> o + 1) k 1 m) Map.empty

hasCount :: Integer -> String -> Bool
hasCount n str = not . null $ Map.filter (== n) $ letterCounts str

part1 :: String -> Integer
part1 input = let ids = words input in
  fromIntegral $ (*) (length $ filter (hasCount 2) ids)
                     (length $ filter (hasCount 3) ids)

oneOff :: String -> String -> Bool
oneOff (x:xs) (y:ys) = if x == y then oneOff xs ys else xs == ys
oneOff [] []         = False

check :: String -> [String] -> [String]
check x xs = filter (\y -> oneOff x y) xs

part2 :: String -> Maybe String
part2 input = (\(a,b) -> intersect a b) <$> go ids
  where
    ids = words input
    go (x:xs) = case check x xs of
        [a] -> Just (x, a)
        []  -> go xs
    go [] = Nothing

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()

