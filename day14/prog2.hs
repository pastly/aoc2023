import Data.Maybe
import Data.List (sort, find, findIndex)
import Debug.Trace
import Data.Maybe (fromJust)
import Data.Function.Memoize
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

remember :: Ord a => a -> State (Set a) ()
remember a = modify (Set.insert a)
haveSeen :: Ord a => a -> State (Set a) Bool
haveSeen a = do seen <- get
                return (a `Set.member` seen)
isDuplicate :: Ord a => a -> State (Set a) Bool
isDuplicate a = do seen <- haveSeen a
                   remember a
                   return seen
takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (a:as) =
  do test <- p a
     if test
     then do rest <- takeWhileM p as
             return (a:rest)
     else return []
takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate as = evalState (takeUntilDuplicate' as) Set.empty
  where takeUntilDuplicate' :: Ord a => [a] -> State (Set a) [a]
        takeUntilDuplicate' = takeWhileM (fmap not . isDuplicate)

type Coord = (Int,Int)

data RockType = Round | Cube
  deriving (Ord, Eq, Show)
deriveMemoizable ''RockType

type Rock = (RockType,Coord)

type Plot = [Rock]

charToRockType :: Char -> Maybe RockType
charToRockType '#' = Just Cube
charToRockType 'O' = Just Round
charToRockType _   = Nothing

charsToXCoordRocks :: [Char] -> [(Int, RockType)]
charsToXCoordRocks string =
    map    (\(x,r) -> (x, fromJust r))
  . filter (\(_,r) -> isJust r) 
  . map    (\(x,c) -> (x, charToRockType c))
  . zip [0..]
  $ string

plotFromMess :: [(Int, [(Int,RockType)])] -> Plot
plotFromMess [] = []
plotFromMess ((y,[]):rest) = plotFromMess rest
plotFromMess ((y,((x,r):xrs)):rest) = (r,(x,y)) : (plotFromMess [(y,xrs)] ++ plotFromMess rest)

-- returns the Y coords of all rocks in the plot that reside on column X. Don't
-- assume anything about the order of the Y coords.
rocksOnX :: Int -> Plot -> [Int]
rocksOnX x plot = ys
  where
    ys = map (\(_,(_,y)) -> y) rocks
    rocks = filter (\(rt,(rx,ry)) -> rx == x) plot

moveRockAt :: Plot -> Coord -> Plot
moveRockAt plot coord = plot'
  where
    idx = fromJust . findIndex (\(rt,c) -> c==coord) $ plot
    (before,_:after) = splitAt idx plot
    rest = before ++ after
    (x,y) = coord
    rockYsAbove = filter (<y) $ rocksOnX x rest
    yAbove = maximum ((-1):rockYsAbove)
    rock' = (Round,(x,yAbove+1))
    plot' = rock' : rest


roundRocks :: Plot -> [Coord]
-- roundRocks plot = map (\(_,c) -> c) . filter (\(rt,_) -> rt==Round) $ plot
roundRocks plot = ans
  where
    ans = map snd . takeWhile (\(rt,_) -> rt==Round) $ plot'
    plot' = sort plot

moveRocksAt :: Plot -> [Coord] -> Plot
moveRocksAt plot [] = plot
moveRocksAt plot (c:cs) = moveRocksAt (moveRockAt plot c) cs

-- calc width and height of plot
wh :: Plot -> (Int,Int)
wh plot = (1 + maximum xs, 1 + maximum ys)
  where
    (xs,ys) = unzip . map snd $ plot

weight :: Plot -> Int
weight plot = sum . map (\(_,y) -> rockWeight y) . roundRocks $ plot
  where
    (w,h) = wh plot
    rockWeight = abs . subtract h

quarterCycle :: Plot -> Plot
quarterCycle plot = plot'
  where
    plot'  = moveRocksAt plot rounds
    rounds = sort . roundRocks $ plot

transposeRock' :: Rock -> Rock
transposeRock' (rt,(x,y)) = (rt,(y,x))

mirrorXRock' :: Int -> Rock -> Rock
mirrorXRock' w (rt,(x,y)) = (rt,(w-x-1,y))

mirrorYRock' :: Int -> Rock -> Rock
mirrorYRock' h (rt,(x,y)) = (rt,(x,h-y-1))

transposeRock = memoize transposeRock'
mirrorXRock = memoize2 mirrorXRock'
mirrorYRock = memoize2 mirrorYRock'

transpose :: Plot -> Plot
transpose plot = map transposeRock plot

-- mirror across a vertical line. I.e. Xs are flipped and Ys stay the same
mirrorX :: Int -> Plot -> Plot
mirrorX w plot = map (mirrorXRock w) plot

-- mirror across a horizontal line. I.e. Ys are flipped and Xs stay the same
mirrorY :: Int -> Plot -> Plot
mirrorY h plot = map (mirrorYRock h) plot

cyclePlot :: (Int,Int) -> Plot -> Plot
cyclePlot (w,h) plot = plot'
  where
    plotAfterN = quarterCycle plot
    plotAfterW = quarterCycle . transpose $ plotAfterN
    plotAfterS = quarterCycle . mirrorY h . transpose $ plotAfterW
    plotAfterE = quarterCycle . transpose . mirrorX w . mirrorY h $ plotAfterS
    plot' = sort . mirrorX w . transpose $ plotAfterE

main' contents = show ans ++ "\n"
  where
    ans = weight finalPlot
    finalPlot = plots !! (nBeforeLoop + (n `mod` loopLen))
    n = 1000000000 - nBeforeLoop
    loopLen = length plots - nBeforeLoop
    nextPlot = cyclePlot (wh plot) (last plots)
    (nBeforeLoop,_) = fromJust . find (\(i,p) -> p==nextPlot) .  zip [0..] $ plots
    plots = takeUntilDuplicate (iterate (cyclePlot (wh plot)) plot)
    plot =
        plotFromMess
      . zip [0..]
      . map charsToXCoordRocks
      . lines
      $ contents

main = interact main'
