-- I reaallllyyy don't think using LCM is a general solution to the problem,
-- but it works for the provided input And does so for everybody, apparently.
--
-- In the general sense, a single start can hit multiple ends and take varied
-- numbers of steps to get to each subsequent end. Also, after reaching an end
-- you've reached before, you might not step off it in the same direction you
-- did last time.
-- 
-- I started trying to account for this, but the simplicity of just finding the
-- LCM of all distances from each start to the first end they hit was becoming
-- me too hard. Do this simple thing, and you get the right answer.
--
-- Seems like the input was crafted s.t. each start goes to one end in a
-- perfectly repeatable cycle, and the length of the direction vector doesn't
-- matter.
--
-- So I am stopping since I solved *this* problem but not the general one.
import Text.Regex.Posix
import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (foldl')

data Dir = L | R
  deriving (Eq, Show)

parseDirs :: String -> [Dir]
parseDirs [] = []
parseDirs (d:ds)
  | d == 'L' = L : parseDirs ds
  | d == 'R' = R : parseDirs ds
  | otherwise = error ("Invalid Dir: " ++ show d)

type Tree = Map.Map Node (Node, Node)
type Node = String
parseLines :: [String] -> Tree
parseLines [] = Map.empty
parseLines (line:ls) = do
  let [n,l,r] = getAllTextMatches (line =~ "[A-Z0-9]+") :: [String]
  Map.union (Map.singleton n (l,r)) (parseLines ls)

travel :: Tree -> [Dir] -> Node -> Int
travel tree dirs start
  | start `elem` ends = 0
  | otherwise = 1 + travel tree dirs' next'
  where
    (d:dirs') = dirs
    ends = onlyEnds . nodes $ tree
    next' = next tree d start

next :: Tree -> Dir -> Node -> Node
next tree dir node
  | dir == L = (\(l,_) -> l) . fromJust . Map.lookup node $ tree
  | dir == R = (\(_,r) -> r) . fromJust . Map.lookup node $ tree

onlyEnds :: [Node] -> [Node]
onlyEnds ns = filter (\n -> last n == 'Z') ns

onlyStarts :: [Node] -> [Node]
onlyStarts ns = filter (\n -> last n == 'A') ns

nodes :: Tree -> [Node]
nodes t = Map.keys t

main' content = show dists ++ "\n" ++ show lcm' ++ "\n"
  where
    lcm' = foldl' lcm (length dirs) dists
    dists = map (travel tree (cycle dirs)) starts
    tree = parseLines . drop 2 . lines $ content
    dirs = parseDirs . head . lines $ content
    ends = onlyEnds . nodes $ tree
    starts = onlyStarts . nodes $ tree

main = interact main'
