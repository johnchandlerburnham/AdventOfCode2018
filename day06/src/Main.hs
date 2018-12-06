module Main where

import qualified Data.Foldable              as F
import           Data.List
import qualified Data.Map.Lazy              as Map
import           Data.Matrix
import           Data.Maybe
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vec
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
data Coordinate = Coordinate { x :: Int, y :: Int } deriving (Eq, Show, Ord)
data Distance =
  Distance {distance :: Int, toCoordinate :: Coordinate } deriving Show
type Grid = Matrix (Maybe Distance)

instance Eq Distance where
  (==) d d' = (==) (distance d) (distance d')

instance Ord Distance where
  compare d d' = compare (distance d) (distance d')

parseCoordinate :: Parser Coordinate
parseCoordinate = do
  x <- L.decimal
  char ','
  space
  y <- L.decimal
  return $ Coordinate x y

parseCoordinates :: Parser [Coordinate]
parseCoordinates = (sepEndBy1 parseCoordinate newline)

manhattan :: Coordinate -> Coordinate -> Int
manhattan (Coordinate x y) (Coordinate x' y') = abs (x - x') + abs (y - y')

mkDistance :: Coordinate -> Coordinate -> Distance
mkDistance a b = Distance (manhattan a b) b

closest :: [Coordinate] -> Coordinate -> Maybe Distance
closest cs c = case (take 2 distances) of
  (d:d':_) -> if d == d' then Nothing else Just $ min d d'
  (d:[])   -> Just d
  _        -> Nothing
  where
  distances = sort $ (mkDistance c) <$> cs

mkGrid1 :: [Coordinate] -> Grid
mkGrid1 cs = matrix width height (\(x,y) -> closest cs' (Coordinate x y))
  where
    topEdge = minimum (y <$> cs)
    leftEdge = minimum (x  <$> cs)
    cs' = (\c -> Coordinate ((x c) - leftEdge) ((y c) - topEdge)) <$> cs
    width = maximum (x <$> cs')
    height = maximum (y <$> cs')

edges :: Grid -> Set.Set Coordinate
edges g = Set.fromList $ toCoordinate <$> (catMaybes $ Vec.toList $ vs)
  where
    vs = Vec.concat $
      [ (getRow 1 g)
      , (getRow ((nrows g)) g)
      , (getCol 1 g)
      , (getCol ((ncols g)) g)
      ]

areas :: Grid -> Map.Map Coordinate Int
areas grid = foldr insert' Map.empty grid
  where
    insert' (Just k) m = Map.insertWith (\n o -> o + 1) (toCoordinate k) 1 m
    insert' Nothing m  = m

maxValue :: Ord b => Map.Map a b -> Maybe (a,b)
maxValue m = Map.foldrWithKey compare' Nothing m
  where
   compare' k v (Just (k', v')) = if v > v' then Just (k, v) else Just (k', v')
   compare' k v Nothing         = Just (k, v)

part1 :: String -> Maybe (Coordinate, Int)
part1 input = do
  cs <- either (const Nothing) Just $ (parse parseCoordinates "" input)
  let g = mkGrid1 cs
  let es = edges g
  maxValue $ Map.filterWithKey (\ k v -> not $ k `Set.member` es) (areas g)

nullCoordinate :: Coordinate
nullCoordinate = Coordinate (-1) (-1)

sumDistance :: [Coordinate] -> Coordinate -> Maybe Distance
sumDistance cs c = let d = sum $ (manhattan c) <$> cs in
  if d <= 10000 then Just (Distance d nullCoordinate) else Nothing

mkGrid2 :: [Coordinate] -> Grid
mkGrid2 cs = matrix width height (\(x,y) -> sumDistance cs' (Coordinate x y))
  where
    average = Coordinate (sum (x <$> cs) `div` n) ((sum (y <$> cs)) `div` n)
    farthest = maximum ((manhattan average) <$> cs)
    topEdge = (y average) - farthest
    leftEdge = (x average) - farthest
    cs' = (\c -> Coordinate ((x c) - leftEdge) ((y c) - topEdge)) <$> cs
    n = length cs
    width = 2 * farthest
    height = 2 * farthest

part2 :: String -> Maybe Int
part2 input = do
  cs <- either (const Nothing) Just $ (parse parseCoordinates "" input)
  let g = mkGrid2 cs
  return $ length $ catMaybes $ F.toList g

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()
