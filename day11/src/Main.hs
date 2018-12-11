module Main where

import           Data.List                  (maximumBy)
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Data

power :: I -> (I, I) -> I
power serial (x,y) = let rID = x + 10 in
  (((rID * y + serial) * rID) `div` 100 `mod` 10) - 5

grid :: I -> Matrix I
grid serial = (300 >< 300) [(power serial (x,y)) | x <- [1..300], y <- [1..300]]

maxPowerOf3 :: I -> (I,I,I)
maxPowerOf3 serial = let g = grid serial in
  maximumBy (\(_,_,p) (_,_,p') -> compare p p') $ do
    x <- [0..297] :: [I]
    y <- [0..297] :: [I]
    let x' = fromIntegral x :: Int
    let y' = fromIntegral y :: Int
    return $(x + 1,y + 1,sumElements $ subMatrix (x',y') (3,3) g)

maxN :: Matrix I -> I -> (I,I,I,I)
maxN g n =
  maximumBy (\(_,_,_,p) (_,_,_,p') -> compare p p') $ do
    x <- [0..(300 - n)]
    y <- [0..(300 - n)]
    let x' = fromIntegral x
    let y' = fromIntegral y
    let n' = fromIntegral n
    return $(x + 1,y + 1, n, sumElements $ subMatrix (x',y') (n',n') g)

maxAll :: I -> (I,I,I,I)
maxAll serial = let g = grid serial in
  maximumBy (\(_,_,_,p) (_,_,_,p') -> compare p p') $ maxN g <$> [1..300]

main :: IO ()
main = do
  print $ maxPowerOf3 5791
  print $ maxAll 5791
  return ()
