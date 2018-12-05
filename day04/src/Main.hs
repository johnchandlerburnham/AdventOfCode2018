module Main where

import           Data.List
import qualified Data.Map.Lazy              as Map
import           Data.Maybe
import           Data.Time
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type GuardID = Int

data Timestamp = Timestamp { year   :: Int
                           , month  :: Int
                           , day    :: Int
                           , hour   :: Int
                           , minute :: Int
                           } deriving (Show, Eq, Ord)

data Watch = Watch { date :: Timestamp , guardID :: Int, sleeps :: [Sleep] } deriving Show
data Start = Start Timestamp GuardID deriving (Eq, Show)
data Sleep = Sleep Timestamp Timestamp deriving (Eq, Show)

parseTimestamp :: Parser Timestamp
parseTimestamp = do
  char '['
  y <- L.decimal
  char '-'
  m <- L.decimal
  char '-'
  d <- L.decimal
  space
  h <- L.decimal
  char ':'
  m' <- L.decimal
  char ']'
  return $ Timestamp y m d h m'

restOfLine :: Parser String
restOfLine = takeWhileP Nothing (/='\n') <* newline

parseStart :: Parser Start
parseStart = do
  t <- parseTimestamp
  string " Guard #"
  gID <- L.decimal
  restOfLine
  return $ Start t gID

parseSleep :: Parser Sleep
parseSleep = do
  t <- parseTimestamp
  string " falls asleep"
  restOfLine
  t' <- parseTimestamp
  string " wakes up"
  restOfLine
  return $ Sleep t t'

parseWatch :: Parser Watch
parseWatch = do
  (Start t g) <- parseStart
  es <- many (try parseSleep)
  return $ Watch (roundToNearestDay t) g es

roundToNearestDay :: Timestamp -> Timestamp
roundToNearestDay t@(Timestamp y m d h m') =
  if h < 12
  then (Timestamp y m d 0 0)
  else (Timestamp y m (d+1) 0 0)

watches :: [Watch] -> Map.Map GuardID [Sleep]
watches = foldr insert'  Map.empty
  where
    insert' w m = Map.insertWith (++) (guardID w) (sleeps w) m

sleepTotal :: Sleep -> Int
sleepTotal (Sleep a b) = minute b - minute a

sleepsTotal :: [Sleep] -> Int
sleepsTotal = foldr (\s x -> sleepTotal s + x) 0

minuteTotals :: [Sleep] -> Map.Map Int Int
minuteTotals ws = go ws Map.empty
  where
    go ((Sleep a b):ss) m =
      go ss (foldr insert' m (enumFromTo (minute a) ((minute b) - 1)))
    go [] m               = m
    insert' minute map = Map.insertWith (\n o -> o + 1) minute 1 map

maxValue :: Ord b => Map.Map a b -> Maybe (a,b)
maxValue m = Map.foldrWithKey compare' Nothing m
  where
   compare' k v (Just (k', v')) = if v > v' then Just (k, v) else Just (k', v')
   compare' k v Nothing         = Just (k, v)

testWatch :: String
testWatch = concat
  [ "[1518-03-11 00:04] Guard #1499 begins shift\n"
  , "[1518-03-11 00:33] falls asleep\n"
  , "[1518-03-11 00:54] wakes up\n"
  , "[1518-03-15 23:50] Guard #1811 begins shift\n"
  , "[1518-03-16 00:03] falls asleep\n"
  , "[1518-03-16 00:44] wakes up\n"
  ]

sortInput :: String -> String
sortInput = (\x -> x ++ "\n") . (intercalate "\n") . sort . lines

part1 :: String -> Maybe Int
part1 input = do
  ws <- watches <$>
    (either (const Nothing) pure $ parse (many parseWatch) "" (sortInput input))
  maxGuard <- fst <$> (maxValue $ sleepsTotal <$> ws)
  mgSleeps <- Map.lookup maxGuard ws
  maxMinute <- fst <$> (maxValue $ minuteTotals mgSleeps)
  return $ maxGuard * maxMinute

(<$$>) = fmap . fmap

part2 :: String -> Maybe Int
part2 input = do
  ws <- watches <$>
    (either (const Nothing) pure $ parse (many parseWatch) "" (sortInput input))
  let minuteMap :: Map.Map GuardID (Maybe (Int, Int))
      minuteMap = maxValue <$> minuteTotals <$> ws
  maxGuard <- fst <$> (maxValue $ snd <$$> minuteMap)
  maxMinute <- Map.lookup maxGuard (fst <$$> minuteMap)
  ((*) maxGuard) <$> maxMinute

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()


