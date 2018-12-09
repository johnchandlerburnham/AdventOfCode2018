module Main where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List
import           Data.Maybe

data Node = Node { header :: (Int, Int), childs :: [Node] , meta :: [Int] }
  deriving (Eq, Show)

metas :: Node -> [Int]
metas (Node _ cs m) = m ++ concatMap metas cs

type Parser = Parsec Void String

parseNode :: Parser Node
parseNode = do
  hc <- L.decimal
  space
  hm <- L.decimal
  space
  cs <- count hc parseNode
  space
  ms <- count hm (L.decimal <* space)
  return $ Node (hc, hm) cs ms

part1 :: String -> Maybe Int
part1 input = do
  n <- either (const Nothing) Just $ (parse parseNode "" input)
  return $ sum $ metas n

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _     = Nothing
safeIndex (x:xs) n
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = safeIndex xs (n - 1)

value :: Node -> Int
value (Node _ [] ms) = sum ms
value (Node _ cs ms) = sum $ value <$> (mapMaybe (\m -> safeIndex cs (m - 1)) ms)

part2 :: String -> Maybe Int
part2 input = do
  n <- either (const Nothing) Just $ (parse parseNode "" input)
  return $ value n

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()
