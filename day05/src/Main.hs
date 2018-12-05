module Main where

import           Data.Char
import           Data.List
import           Debug.Trace

data Zip a = Zip { ls :: [a], rs :: [a]} deriving (Eq, Show)

fromString ::  String -> Zip Char
fromString str = Zip "" str

toString :: Zip Char -> String
toString (Zip (l:ls) rs) = toString (Zip ls (l:rs))
toString (Zip "" rs)     = rs

opposites :: Char -> Char -> Bool
opposites x y = toUpper x == y && toLower y == x
             || toUpper y == x && toLower x == y

reduce :: Zip Char -> String
reduce z@(Zip (l:ls) (r:rs)) =
  if opposites l r
  then --trace (show z) $
    reduce (Zip ls rs)
  else -- trace (show z) $
    reduce (Zip (r:l:ls) rs)
reduce z@(Zip "" (r:rs))        = --trace (show z) $
  reduce (Zip (r:[]) rs)
reduce z@(Zip ls "")          = toString z

part1 :: String -> Int
part1 input = length (reduce $ fromString input) - 1 -- don't count the \n

filterInput :: Char -> String -> String
filterInput c = filter (\x -> toLower x /= toLower c)

part2 :: String -> [(Char, Int)]
part2 input = let cs = (nub $ map toLower input)
                  dec x = x - 1
              in
  zip cs (dec . length . reduce . fromString . (flip filterInput input) <$> cs)


main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()


