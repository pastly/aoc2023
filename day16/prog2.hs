import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent
import Control.Parallel.Strategies
import Data.Function.Memoize
import Data.List
import Data.Maybe
import Debug.Trace

data Dir = N | S | E | W
  deriving (Ord,Eq,Show)
deriveMemoizable ''Dir
type Coord = (Int,Int)
type Mirror = Char

type Graph = M.Map (Coord,Dir) Coord
type Touched = S.Set Coord

data Plot = Plot {
    byX :: [[(Int,Mirror)]],
    byY :: [[(Int,Mirror)]],
    w :: Int,
    h :: Int
  } deriving (Show)
deriveMemoizable ''Plot

plotFromMess :: [(Int, [(Int,Mirror)])] -> Plot
plotFromMess mess = Plot (byX ms) (byY ms) w h
  where
    ms = plotFromMess' mess
    (w,h) = (\(xs, ys) -> (maximum xs + 1, maximum ys + 1)) . unzip . M.keys $ ms
    plotFromMess' :: [(Int, [(Int,Mirror)])] -> M.Map Coord Mirror
    plotFromMess' [] = M.empty
    plotFromMess' ((y,[]):rest) = plotFromMess' rest
    plotFromMess' ((y,((x,'.'):xms)):rest) = plotFromMess' [(y,xms)] `M.union` plotFromMess' rest
    plotFromMess' ((y,((x,m):xms)):rest) = M.unions [M.singleton (x,y) m, plotFromMess' [(y,xms)],  plotFromMess' rest]
    byX :: M.Map Coord Mirror -> [[(Int,Mirror)]]
    byX ms = foldl' byX' (replicate w []) (M.assocs ms)
    byX' :: [[(Int,Mirror)]] -> (Coord,Mirror) -> [[(Int,Mirror)]]
    byX' cache ((x,y),m) = take x cache ++ (((y,m):cache!!x) : drop (x+1) cache)
    byY :: M.Map Coord Mirror -> [[(Int,Mirror)]]
    byY ms = foldl' byY' (replicate h []) (M.assocs ms)
    byY' :: [[(Int,Mirror)]] -> (Coord,Mirror) -> [[(Int,Mirror)]]
    byY' cache ((x,y),m) = take y cache ++ (((x,m):cache!!y) : drop (y+1) cache)

nextInDir :: Plot -> Dir -> Coord -> Maybe (Coord,Mirror)
nextInDir plot dir (x,y)
  | dir == N && isNothing yAbove  = Nothing
  | dir == N                      = Just ((x,fst.fromJust$yAbove),snd.fromJust$yAbove)
  | dir == S && isNothing yBelow  = Nothing
  | dir == S                      = Just ((x,fst.fromJust$yBelow),snd.fromJust$yBelow)
  | dir == W && isNothing xLeft   = Nothing
  | dir == W                      = Just ((fst.fromJust$xLeft,y),snd.fromJust$xLeft)
  | dir == E && isNothing xRight  = Nothing
  | dir == E                      = Just ((fst.fromJust$xRight,y),snd.fromJust$xRight)
  where
    yAbove
      | length ysAbove == 0 = Nothing
      | otherwise = Just (last ysAbove)
    yBelow
      | length ysBelow == 0 = Nothing
      | otherwise = Just (head ysBelow)
    xLeft
      | length xsLeft == 0 = Nothing
      | otherwise = Just (last xsLeft)
    xRight
      | length xsRight == 0 = Nothing
      | otherwise = Just (head xsRight)
    ysAbove = sort $ filter (\(i,_) -> i<y) ((byX plot)!!x)
    ysBelow = sort $ filter (\(i,_) -> i>y) ((byX plot)!!x)
    xsLeft  = sort $ filter (\(i,_) -> i<x) ((byY plot)!!y)
    xsRight = sort $ filter (\(i,_) -> i>x) ((byY plot)!!y)

walk :: (Dir -> Coord -> Maybe (Coord,Mirror)) -> Plot -> Graph -> Touched -> Dir -> Coord -> (Touched, Graph)
walk nid plot graph touched dir pos = (S.union touched' touched'', graph'')
  where
    (touched'', graph'')
      | (pos,dir) `M.member` graph = (touched, graph)
      | isNothing next = (touched', graph)
      | otherwise = case (dir, nMir) of
        (N, '|')  -> walk nid plot graph' touched' dir nCoord
        (S, '|')  -> walk nid plot graph' touched' dir nCoord
        (_, '|')  -> (\(ts, gs) -> (S.unions ts, M.unions gs))
                     $ unzip [(walk nid plot graph' touched' N nCoord), (walk nid plot graph' touched' S nCoord)]
        (E, '-')  -> walk nid plot graph' touched' dir nCoord
        (W, '-')  -> walk nid plot graph' touched' dir nCoord
        (_, '-')  -> (\(ts, gs) -> (S.unions ts, M.unions gs))
                     $ unzip [(walk nid plot graph' touched' E nCoord), (walk nid plot graph' touched' W nCoord)]
        (N, '/')  -> walk nid plot graph' touched' E nCoord
        (S, '/')  -> walk nid plot graph' touched' W nCoord
        (E, '/')  -> walk nid plot graph' touched' N nCoord
        (W, '/')  -> walk nid plot graph' touched' S nCoord
        (N, '\\') -> walk nid plot graph' touched' W nCoord
        (S, '\\') -> walk nid plot graph' touched' E nCoord
        (E, '\\') -> walk nid plot graph' touched' S nCoord
        (W, '\\') -> walk nid plot graph' touched' N nCoord
    next = nid dir pos
    (nCoord,nMir) = fromJust next
    graph' = M.insert (pos,dir) nCoord graph
    (posX,posY) = pos
    touched'
      | isNothing next = case dir of
        N -> S.union touched (pointsBetween pos (posX,0))
        S -> S.union touched (pointsBetween pos (posX,(h plot-1)))
        W -> S.union touched (pointsBetween pos (0,posY))
        E -> S.union touched (pointsBetween pos ((w plot-1),posY))
      | otherwise = S.union touched (pointsBetween pos nCoord)

pointsBetween :: Coord -> Coord -> Touched
pointsBetween (x1,y1) (x2,y2)
  | x1 < x2 = S.fromList (zip [x1..x2] (repeat y1))
  | x1 > x2 = S.fromList (zip [x2..x1] (repeat y1))
  | y1 < y2 = S.fromList (zip (repeat x1) [y1..y2])
  | y1 > y2 = S.fromList (zip (repeat x1) [y2..y1])
  | otherwise = S.singleton (x1,y1)

allStarts :: Plot -> [(Coord,Dir)]
allStarts plot = downs ++ ups ++ lefts ++ rights
  where
    downs = zip (zip xs (repeat 0)) (repeat S)
    ups = zip (zip xs (repeat (h plot-1))) (repeat N)
    lefts = zip (zip (repeat 0) ys) (repeat E)
    rights = zip (zip (repeat (w plot-1)) ys) (repeat W)
    xs = [0..(w plot-1)]
    ys = [0..(h plot-1)]

main' contents = show ans ++ "\n"
  where
    ans =
        maximum
      . parMap  rpar (\(t,_) -> length t)
      . parMap rpar (\(s,d) -> walk nid plot graph touched d s)
      . allStarts
      $ plot
    nid = memoize2 (nextInDir plot)
    touched = S.empty
    graph = M.empty
    plot =
        plotFromMess
      . zip [0..]
      . map (zip [0..])
      . lines
      $ contents

main = interact main'
