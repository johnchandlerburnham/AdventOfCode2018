module Main where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Control.Monad
import qualified Control.Monad.State.Lazy   as ST
import           Data.Array
import           Data.Char                  (chr, ord)
import           Data.Graph
import           Data.List                  (groupBy, intercalate, minimumBy,
                                             sort, sortBy)
import qualified Data.Set                   as Set
import           Data.Tree
import           Debug.Trace


type Parser = Parsec Void String

parseEdge :: Parser Edge
parseEdge = do
  string "Step "
  [a] <- takeP Nothing 1
  string " must be finished before step "
  [b] <- takeP Nothing 1
  string " can begin."
  return $ (ord a, ord b)

parseEdges :: Parser [Edge]
parseEdges = sepEndBy1 parseEdge newline

inn :: Graph -> Vertex -> [Vertex]
inn g v = fst <$> (filter (\(i, o) -> o == v) $ edges g)

out :: Graph -> Vertex -> [Vertex]
out g v = snd <$> (filter (\(i, o) -> i == v) $ edges g)

isUnlocked :: Graph -> Vertex -> [Vertex] -> Bool
isUnlocked g v visiteds = all (\x -> elem x visiteds) (inn g v)

unlocked :: Graph -> [Vertex] -> [Vertex]
unlocked g visiteds = filter (\v -> isUnlocked g v visiteds) (vertices g)

unvisited :: [Vertex] -> [Vertex] -> [Vertex]
unvisited visiteds vs = filter (\x -> not $ elem x visiteds) vs

nextVertex :: Graph -> [Vertex] -> Vertex
nextVertex g visiteds = minimum $ unvisited visiteds $ unlocked g visiteds

order :: Graph -> Vertex -> String
order g root = chr <$> go [root]
  where
   size = length $ vertices g
   go vs = if length vs == size then vs else go ((nextVertex g vs):vs)

buildGWithRoot :: Bounds -> [Edge] -> (Graph, Vertex)
buildGWithRoot (m,n) es = (buildG (root, n) es', root)
  where
   g = buildG (m, n) es
   zeroIns = filter (\(a,b) -> b == 0) $ assocs $ indegree g
   root = m - 1
   es' = es ++ ((\(a,b) -> (root,a)) <$> zeroIns)

part1 :: String -> Maybe String
part1 input = do
  es <- either (const Nothing) Just $ (parse parseEdges "" input)
  let (g, r) = buildGWithRoot (ord 'A', ord 'Z') es
  return $ reverse $ order g r

data Worker = Idle | Worker { on :: Vertex, time :: Int } deriving (Eq, Show)
data GraphST = GraphST
  { graph     :: Graph
  , totalTime :: Int
  , workers   :: [Worker]
  , done      :: [Vertex]
  } deriving Eq

instance Show GraphST where
  show (GraphST _ t w d) = intercalate " " [(show t), (show w), (show d)]

workingOn :: [Worker] -> [Vertex]
workingOn ws = on <$> (filter ((/=) Idle) ws)

duration :: Vertex -> Int
duration v  = 60 + (v - 64)

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just $ minimum xs

newWorker :: Graph -> [Worker] -> [Vertex] -> Worker
newWorker g ws vs = case (unvisited ((workingOn ws) ++ vs) $ unlocked g vs) of
  []  -> Idle
  vs' -> Worker (minimum vs') 1

tickWorker :: Worker -> ST.State GraphST Worker
tickWorker Idle = do
  (GraphST g t ws vs) <- ST.get
  let w' = newWorker g ws vs
  ST.put (GraphST g t (w':ws) vs)
  return $ w'
tickWorker w@(Worker v t) = do
  (GraphST g t' ws vs) <- ST.get
  if t == duration v
  then do
    let w' = newWorker g ws (v:vs)
    ST.put (GraphST g t' (w':ws) (v:vs))
    return $ w'
  else return $ (Worker v (t + 1))

tick :: GraphST -> GraphST
tick gST@(GraphST g tT ws vs) =
  let (ws', gST') = ST.runState (sequence $ tickWorker <$> ws) gST
  in GraphST g (tT + 1) ws' (done gST')

workFromRoot :: Graph -> Vertex -> GraphST
workFromRoot g root = GraphST g 0 (replicate 5 Idle) [root]

loop :: GraphST -> Int
loop gST@(GraphST g t ws vs) = trace msg $
  if length (vertices g) == length vs
  then t - 1
  else loop $ tick gST
  where
    msg = intercalate "\n"
      [ (show gST )
      , (show $ unlocked g vs)
      , (show $ (unvisited vs $ unlocked g vs))
      , (show $ (unvisited ((workingOn ws) ++ vs) $ unlocked g vs))
      ]

part2 :: String -> Maybe Int
part2 input = do
  es <- either (const Nothing) Just $ (parse parseEdges "" input)
  let (g, r) = buildGWithRoot (ord 'A', ord 'Z') es
  return $ loop (workFromRoot g r)

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input
  return ()
