module Main where

import           Control.Monad.Combinators
import           Data.Foldable
import qualified Data.Map.Lazy              as Map
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Coordinate = (Integer, Integer)
type Fabric = Map.Map Coordinate Integer

data Claim = Claim { idn   :: Integer
                   , start :: Coordinate
                   , size  :: (Integer, Integer)
                   } deriving (Eq, Show)

type Parser = Parsec Void String

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  idn <- L.decimal
  space
  char '@'
  space
  x <- L.decimal
  char ','
  y <- L.decimal
  char ':'
  space
  w <- L.decimal
  char 'x'
  h <- L.decimal
  return $ Claim idn (x, y) (w,h)

parseClaims :: Parser [Claim]
parseClaims = parseClaim `sepEndBy` newline

coordinates :: Claim -> [Coordinate]
coordinates (Claim _ (x,y) (w,h)) =
  [ (a,b) | a <- [x..(x+w-1)], b <- [y..(y+h-1)] ]

fabric :: [Coordinate] -> Fabric
fabric = foldl' (\ m k -> Map.insertWith (\n o -> o + 1) k 1 m) Map.empty

overlapping :: Fabric -> Integer
overlapping f = fromIntegral $ Map.size $ Map.filter (>=2) f

noOverlap :: Fabric -> Claim -> Bool
noOverlap f c = all (\x -> x <= (Just 1)) $
  (\k -> Map.lookup k f) <$> coordinates c

part1 :: String -> Integer
part1 input = overlapping $ fabric $ concatMap coordinates claims
  where
    claims = either (const []) id $ parse parseClaims "" input

part2 :: String -> [Integer]
part2 input = idn <$> filter (noOverlap fabr) claims
  where
    fabr = fabric $ concatMap coordinates claims
    claims = either (const []) id $ parse parseClaims "" input

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()

