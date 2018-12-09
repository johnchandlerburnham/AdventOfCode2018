module Main where

import Data.CircularList
import Data.Maybe (fromJust)

import Debug.Trace

type Players = CList (Int,Int)
type Marbles = CList Int

data Game = Game
  { players :: Players
  , marbles :: Marbles
  , next :: Int
  }

-- right clockwise
-- left counter

instance Show Game where
  show (Game ps ms n) = (show n) ++ ":\t" ++ (show $ toList ms) ++ "\n\t" ++ (show $ toList ps)

newPlayers :: Int -> CList (Int, Int)
newPlayers n = fromList $ zip [1..n] (replicate n 0)

newGame :: Int -> Game
newGame n = Game (newPlayers n) empty 0

turn :: Game -> Game
turn g@(Game ps ms m')
  | m' /= 0 && m' `mod` 23 == 0 = turn23 g
  | otherwise = Game (rotR ps) (insertL m' (rotR ms)) (m' + 1)

turn23 :: Game -> Game
turn23 g@(Game ps ms m') =
  let ms' = (rotNL 7 ms)
      m2 = fromJust $ focus ms'
      ms''  = removeR ms'
      (p, s) = fromJust $ focus ps
  in Game (rotR $ update (p, (s + m' + m2)) ps) ms'' (m' + 1)

game :: Int -> Int -> Game
game players marbles = go (newGame players)
  where
    go g@(Game ps ms m) = -- trace (show g) $
      if m == marbles then (turn g) else go (turn g)

highScore :: Game -> Int
highScore g = maximum $ snd <$> (toList $ players g)

main :: IO ()
main = do
  --print $ game 9 25
  --print $ highScore $ game 10 1618
  print $ highScore $ game 410 72059
  print $ highScore $ game 410 7205900
