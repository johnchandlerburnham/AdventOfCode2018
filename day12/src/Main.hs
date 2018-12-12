module Main where

import           Control.Comonad

data Row a = Row
  { index :: Int
  , gen   :: Int -> a
  }

instance Show a => Show (Row a) where
  show row@(Row i g) = "Row " ++ (show i) ++ " " ++ (summarize 12 row)

summarize :: Show a => Int -> Row a -> String
summarize n row = let (a, b) = toInfiniteLists row in
  "left: " ++ (show $ take n a) ++ "..., right: " ++ (show $ take n b) ++ "..."

current :: Row a -> a
current (Row i g) = g i

fromGen :: (Int -> a) -> Row a
fromGen g = Row 0 g

moveLeft :: Row a -> Row a
moveLeft (Row i g) = Row (i - 1) g

moveRight :: Row a -> Row a
moveRight (Row i g) = Row (i + 1) g

moveLeftN :: Int -> Row a -> Row a
moveLeftN n (Row i g) = Row (i - n) g

moveRightN :: Int -> Row a -> Row a
moveRightN n (Row i g) = Row (i + n) g

toInfiniteLists :: Row a -> ([a], [a])
toInfiniteLists row = (,) (current <$> (iterate moveLeft row))
                         (current <$> (iterate moveRight row))

safeIndex :: Monoid a => Int -> [a] -> a
safeIndex n l
  | n < 0 = mempty
  | n >= length l = mempty
  | otherwise = l !! n

fromList :: Monoid a => [a] -> Row a
fromList l = Row 0 $ \n -> safeIndex n l

fromList' :: Monoid a => Int -> [a] -> Row a
fromList' i l = Row i $ \n -> safeIndex (n - i) l

toList :: Int -> Int -> Row a -> [a]
toList min max (Row i g) = g <$> [min..max]

shiftGen :: (Int -> a) -> Int -> (Int -> a)
shiftGen g n = \i -> g (i + n)

instance Functor Row where
  fmap f (Row i g) = Row i (f . g)

instance Comonad Row where
  extract = current
  duplicate (Row i g) = Row i (\n -> Row (i + n) g)

example :: Int -> Int
example n
  | n < 0 = -1
  | n < (length l) = l !! n
  | otherwise = 1
  where
    l = [2..10]

data Pot = T | F deriving (Eq, Show)

instance Semigroup Pot where
  (<>) T _ = T
  (<>) _ T = T
  (<>) _ _ = F

instance Monoid Pot where
  mempty = F

data Neighborhood = N Pot Pot Pot Pot Pot

getNeighborhood :: Row Pot -> Neighborhood
getNeighborhood p = N l' l c r r'
  where
    c = current p
    l = current $ moveLeft p
    l' = current $ moveLeftN 2 p
    r = current $ moveRight p
    r' = current $ moveRightN 2 p

rules :: Neighborhood -> Pot
rules (N T T F T T) = T
rules (N F F F F T) = F
rules (N F T F T F) = T
rules (N F F T T T) = F
rules (N T T F F F) = T
rules (N T T T T T) = F
rules (N T T T F T) = T
rules (N F T T F F) = F
rules (N F F T T F) = F
rules (N F F F T T) = T
rules (N T T T T F) = F
rules (N T T T F F) = F
rules (N F T T T T) = T
rules (N T F F F T) = T
rules (N F F F F F) = F
rules (N F F T F F) = F
rules (N T F F T T) = F
rules (N T F T F T) = T
rules (N F T F T T) = T
rules (N F T T T F) = F
rules (N T T F F T) = F
rules (N F T F F F) = T
rules (N F T F F T) = T
rules (N F F F T F) = F
rules (N T F T F F) = F
rules (N T F F F F) = F
rules (N T T F T F) = F
rules (N T F T T T) = F
rules (N F T T F T) = F
rules (N T F F T F) = T
rules (N F F T F T) = F
rules (N T F T T F) = T

testInput :: [Pot]
testInput = [F,F,F,T,F,F,T,F,T,F,F,T,T,F,F,F,F,F,F,T,T,T,F,F,F,T,T,T,F,F,F,F,F,F,F,F,F,F,F]

test = fromList' (-3) testInput

testRules :: Neighborhood -> Pot
testRules (N F F F T T) = T
testRules (N F F T F F) = T
testRules (N F T F F F) = T
testRules (N F T F T F) = T
testRules (N F T F T T) = T
testRules (N F T T F F) = T
testRules (N F T T T T) = T
testRules (N T F T F T) = T
testRules (N T F T T T) = T
testRules (N T T F T F) = T
testRules (N T T F T T) = T
testRules (N T T T F F) = T
testRules (N T T T F T) = T
testRules (N T T T T F) = T
testRules _             = F

testNextRow :: Row Pot -> Row Pot
testNextRow row = extend (testRules . getNeighborhood) row

input :: [Pot]
input =[T,T,F,T,T,T,T,F,F,T,T,T,T,F,F,F,T,F,T,T,T,T,F,F,T,T,F,T,F,F,T,T,F,F,T
  ,T,T,T ,T,F,T,T,F,T,F,F,T,F,F,F,T,F,T,T,T,F,T,T,T,F,F,F,F,T,T,T,T,F,T,T,T,F
  ,F,F,T,T,F,F,T,F,F,F,T,T,F,T,F,T,F,F,F,T,T,F,T,T,F,F
  ]

inputRow :: Row Pot
inputRow = fromList input

nextRow :: Row Pot -> Row Pot
nextRow row = extend (rules . getNeighborhood) row

generation :: Int -> Int -> Int -> (Row Pot -> Row Pot) -> Row Pot -> [Row Pot]
generation n min max next row = go 0 row []
  where
    go n' r rs = let r' = center $ prune r in
      if n' == n then (r':rs) else go (n'+1) (next $ zero r') (r':rs)
    prune r = toList min max r
    center ls = fromList' min ls
    zero r = Row 0 (gen r)

prettyPrint :: [Pot] -> String
prettyPrint []     = ""
prettyPrint (T:ps) = '#':prettyPrint ps
prettyPrint (F:ps) = '.':prettyPrint ps

prettyGens :: [[Pot]] -> IO ()
prettyGens ps = sequence_ ((putStrLn . prettyPrint) <$> ps)

prettyRows :: Int -> Int -> Int -> IO ()
prettyRows n min max = prettyGens $ reverse $ toList min max <$> gens n min max

indicesWithPots :: Int -> Int -> Row Pot -> [Int]
indicesWithPots min max r = (filter (\i -> (gen r) i == T) [min..max])

tg n min max = generation n min max testNextRow test

tg20 :: Int -> Int -> [Row Pot]
tg20 min max = generation 20 min max testNextRow test

gens n min max = generation n min max nextRow inputRow

g20 :: Int -> Int -> [Row Pot]
g20 min max = generation 20 min max nextRow inputRow

nPots :: Int -> Int -> Int -> Int
nPots n min max = sum $ indicesWithPots min max $ head $ gens n min max

-- part 2

trying :: IO ()
trying = do
  -- prettyRows 300 (-5) 350
  let a = (\n -> nPots n (-5) 350) <$> [200..205]
  print $ zipWith (-) a (0:a)
  print $ 6801 + 32 * (50000000000 - 200)
  return ()

main :: IO ()
main = do
  print $ nPots 20 (-5) 120
  trying
  return ()
