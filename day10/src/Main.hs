module Main where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List
import           Data.Matrix
import           Debug.Trace

type Parser = Parsec Void String

data Light = Light { x :: Int, y :: Int, dx :: Int, dy :: Int }

instance Show Light where
  show (Light x y dx dy) =
    "{P:" ++ (show (x,y)) ++ " Δ:" ++ (show (dx,dy)) ++ "}"

signed :: Parser Int
signed = L.signed (return ()) L.decimal

symbol = L.symbol space

parseLight :: Parser Light
parseLight = do
  symbol "position=<"
  x <- signed
  symbol ","
  y <- signed
  symbol "> velocity=<"
  dx <- signed
  symbol ","
  dy <- signed
  string ">"
  return $ Light x y dx dy

parseLights :: Parser [Light]
parseLights = parseLight `sepEndBy` newline

tick :: Light -> Light
tick (Light x y dx dy) = Light (x + dx) (y + dy) dx dy

height :: [Light] -> Int
height ls = let ys = y <$> ls in abs ((maximum ys) - (minimum ys))

width  :: [Light] -> Int
width ls = let xs = x <$> ls in abs ((maximum xs) - (minimum xs))

loop :: (Int, [Light]) -> (Int, [Light])
loop (t, ls) =
  let h = height ls
      ls' = tick <$> ls
      h' =  height ls'
  in if (h - h') < 0 then (t,ls) else loop (t + 1, ls')

trace' = Debug.Trace.trace

data L = X | O deriving Eq

instance Show L where
  show X = "■"
  show O = " "

lightMatrix :: [Light] -> Matrix L
lightMatrix ls =
  let minY = minimum (y <$> ls)
      minX = minimum (x <$> ls)
      ls' = (\(Light x y dx dy) -> (x - (minX - 1), y - (minY - 1))) <$> ls
      rows = 1 + (maximum $ snd <$> ls')
      cols = 1 + (maximum $ fst <$> ls')
   in matrix rows cols (\(a,b) -> if (b,a) `elem` ls' then X else O)

part1 :: String -> Maybe (Int, Matrix L)
part1 input = do
  ls <- either (const Nothing) Just $ (parse parseLights "" input)
  return $ lightMatrix <$> loop (0,ls)

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
--  putStrLn "Part 2:"
--  print $ part2 input return ()
