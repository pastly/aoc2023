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

step :: (Int,Int) -> Grid -> Coord -> Solves -> [(Coord,Dir)] -> Int
step (w,h) grid end solves ((pos,dir):atts) = traceShow nexts 0
  where
    nexts =
        -- list of ((Coord,Dir),Weight)
        map (\(_,(p,d)) -> ( (p,d), weight+(grid M.! p)))
        -- list of (Bool,(Coord,Dir) ), keeping just oks
      . filter (\(ok,_) -> ok) 
      $ [(okW,((posX-1,posY  ),W)),
         (okE,((posX+1,posY  ),E)),
         (okN,((posX  ,posY-1),N)),
         (okS,((posX  ,posY+1),S))]
    okW = dir /= E && posX > 0   && ((posX-1,posY),W) `M.notMember` solves
    okE = dir /= W && posX < w-1 && ((posX+1,posY),E) `M.notMember` solves
    okN = dir /= S && posY > 0   && ((posX,posY-1),N) `M.notMember` solves
    okS = dir /= N && posY < h-1 && ((posX,posY+1),S) `M.notMember` solves
    weight = solves M.! (pos,dir)
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

main' contents = show ans ++ "\n"
  where
    ans = step (w,h) grid end solves atts
    atts = [(start,E), (start,S)]
    solves = M.fromList . map (\a -> (a,0)) $ atts
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
