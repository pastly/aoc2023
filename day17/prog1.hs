import Debug.Trace
import Data.List
import Data.Char
import Data.Maybe
import Control.Concurrent
import Control.Parallel.Strategies
import qualified Data.Map as M

data Dir = W | E | N | S
  deriving (Ord,Eq,Show)
type Weight = Int
type Coord = (Int,Int)
type Grid = M.Map (Coord) Weight
type Path = [(Weight,(Coord,Dir))]
type Solves = M.Map (Coord,Dir) Weight
type Attempts = [Path]

gridFromMess :: [(Int, [(Int,Char)])] -> Grid
gridFromMess [] = M.empty
gridFromMess ((y,[]):rest) = gridFromMess rest
gridFromMess ((y,((x,w):xws)):rest) =
  M.unions [
    M.singleton (x,y) (ord w - 48),
    gridFromMess [(y,xws)],
    gridFromMess rest]

wh :: Grid -> (Int,Int)
wh g = (\(xs, ys) -> (maximum xs+1, maximum ys+1)) . unzip . M.keys $ g

okStraightLim :: Int -> Path -> Dir -> Bool
okStraightLim max path dir
  | length prevs < max = True
  | otherwise          = not . all (==dir) $ prevs
  where
    prevs = map (snd . snd) . take max $ path

-- step :: (Int,Int) -> Grid -> Coord -> Solves -> Attempts -> Path
-- step (w,h) grid end solves atts
--   | areDone = donePath
--   | otherwise = step (w,h) grid end solves' atts''
--   where
--     doneWeight = sum . map fst $ donePath
--     donePath = head atts''
--     --areDone = traceShow (map (fst . snd ) $ head atts'') (\(w,(c,d)) -> c == end) . head . head $ atts''
--     areDone = (\(w,(c,d)) -> c == end) . head . head $ atts''
--     -- save the next steps in solves
--     solves' = foldl' (\out (w,n) -> M.insertWith (\a b -> minimum [a,b]) n w out) solves nexts
--     -- solves' = M.union solves . M.fromList . map (\(w,n) -> (n,w)) $ nexts
--     -- recombine all old paths with the new path(s) we just added to, sorting
--     -- them so the best is first.
--     atts'' = sort (paths ++ atts')
--     paths =
--         -- prepend each next step (Weight,(Coord,Dir)) to copies of the
--         -- existing best path
--         traceShow (show (pos,dir) ++ " to " ++ show nexts) map (\n -> n:path) nexts
--         --map (\n -> n:path) nexts
--     nexts =
--         -- list of (Weight, (Coord,Dir))
--         map (\(_,(p,d)) -> ( weight+(grid M.! p), (p,d)))
--         -- list of (Bool,(Coord,Dir) ), keeping just oks
--       . filter (\(ok,_) -> ok) 
--       $ [(okW,((posX-1,posY  ),W)),
--          (okE,((posX+1,posY  ),E)),
--          (okN,((posX  ,posY-1),N)),
--          (okS,((posX  ,posY+1),S))]
--     okW = dir /= E && okStraightLim' W && posX > 0   && ((posX-1,posY),W) `M.notMember` solves
--     okE = dir /= W && okStraightLim' E && posX < w-1 && ((posX+1,posY),E) `M.notMember` solves
--     okN = dir /= S && okStraightLim' N && posY > 0   && ((posX,posY-1),N) `M.notMember` solves
--     okS = dir /= N && okStraightLim' S && posY < h-1 && ((posX,posY+1),S) `M.notMember` solves
--     (path:atts') = atts
--     okStraightLim' = okStraightLim 3 path
--     (weight,(pos,dir)) = head path
--     (posX,posY) = pos

step :: (Int,Int) -> Grid -> Coord -> Solves -> Attempts -> Path
step (w,h) grid end solves (path:atts)
  | areDone = donePath
  | otherwise = step (w,h) grid end solves' atts''
  where
    doneWeight = sum . map fst $ donePath
    donePath = head atts'
    --areDone = traceShow (map (fst . snd ) $ head atts') (\(w,(c,d)) -> c == end) . head . head $ atts'
    areDone = (\(w,(c,d)) -> c == end) . head . head $ atts'
    -- save the next steps in solves
    solves' = foldl' (\out (w,n) -> M.insertWith (\a b -> minimum [a,b]) n w out) solves nexts
    -- solves' = M.union solves . M.fromList . map (\(w,n) -> (n,w)) $ nexts
    -- recombine all old paths with the new path(s) we just added to, sorting
    -- them so the best is first.
    atts'' = traceShow (map head atts') atts'
    atts' = sort (paths ++ atts)
    paths =
        -- prepend each next step (Weight,(Coord,Dir)) to copies of the
        -- existing best path
        traceShow ("(" ++ show (1+length atts) ++ ") " ++ show path ++ " to " ++ show nexts) map (\n -> n:path) nexts
        --traceShow (show (pos,dir) ++ " to " ++ show nexts) map (\n -> n:path) nexts
        --map (\n -> n:path) nexts
    nexts =
        -- list of (Weight, (Coord,Dir))
        map (\(_,(p,d)) -> ( weight+(grid M.! p), (p,d)))
        -- list of (Bool,(Coord,Dir) ), keeping just oks
      . filter (\(ok,_) -> ok) 
      $ [(okW,((posX-1,posY  ),W)),
         (okE,((posX+1,posY  ),E)),
         (okN,((posX  ,posY-1),N)),
         (okS,((posX  ,posY+1),S))]
    okW = dir /= E && okStraightLim' W && posX > 0   && ((posX-1,posY),W) `M.notMember` solves
    okE = dir /= W && okStraightLim' E && posX < w-1 && ((posX+1,posY),E) `M.notMember` solves
    okN = dir /= S && okStraightLim' N && posY > 0   && ((posX,posY-1),N) `M.notMember` solves
    okS = dir /= N && okStraightLim' S && posY < h-1 && ((posX,posY+1),S) `M.notMember` solves
    okStraightLim' = okStraightLim 3 path
    (weight,(pos,dir)) = head path
    (posX,posY) = pos

drawPath :: (Int,Int) -> M.Map Coord Dir -> String
drawPath (w,h) path = s' ++ "\n"
  where
    charAt x y
      | x >= w = '\n'
      | otherwise = case (x,y) `M.lookup` path of
          Nothing -> '.'
          Just W -> '<'
          Just E -> '>'
          Just N -> '^'
          Just S -> 'v'
    s' = intercalate "\n" s
    s = map (\y -> map (\x -> charAt x y) [0..w-1]) [0..h-1]

main' contents = aaaa ++ "\n" ++ show ans ++ "\n"
  where
    ans = head . map fst $ path
    aaaa = drawPath (w,h) (M.fromList . map snd $ path)
    path = step (w,h) grid end solves atts
    atts = [[(0,(start,E))], [(0,(start,S))]]
    solves = M.fromList . map (\(a,b) -> (b,a)) $ head atts
    start = (0,0)
    end = (w-1,h-1)
    --end = ((w-3),2)
    (w,h) = wh grid
    grid =
        gridFromMess
      . zip [0..]
      . map (zip [0..])
      . lines
      $ contents

main = interact main'
