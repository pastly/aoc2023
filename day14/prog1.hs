import Data.Maybe
import Data.List
import Debug.Trace

type Coord = (Int,Int)

data RockType = Cube | Round
  deriving (Eq, Show)

type Rock = (Coord,RockType)

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
plotFromMess ((y,((x,r):xrs)):rest) = ((x,y),r) : (plotFromMess [(y,xrs)] ++ plotFromMess rest)

-- returns the Y coords of all rocks in the plot that reside on column X. Don't
-- assume anything about the order of the Y coords.
rocksOnX :: Int -> Plot -> [Int]
rocksOnX x plot = ys
  where
    ys = map (\((_,y),_) -> y) rocks
    rocks = filter (\((rx,ry),rt) -> rx == x) plot

moveRockAt :: Plot -> Coord -> Plot
moveRockAt plot coord
  | length rocksAtCoord == 0 = plot
  | rt == Cube               = plot
  | otherwise = plot'
  where
    rocksAtCoord = filter (\(c,rt) -> c==coord) plot
    rock = head rocksAtCoord
    rt = snd rock
    (x,y) = coord
    rockYsAbove = filter (<y) $ rocksOnX x plot
    yAbove
      | length rockYsAbove == 0 = -1
      | otherwise = last . sort $ rockYsAbove
    rock' = ((x,yAbove+1), rt)
    plot' = rock' : filter (\(c,_) -> c/=coord) plot

roundRocks :: Plot -> [Coord]
roundRocks plot = map (\(c,_) -> c) . filter (\(_,rt) -> rt==Round) $ plot

moveRocksAt :: Plot -> [Coord] -> Plot
moveRocksAt plot [] = plot
moveRocksAt plot (c:cs) = moveRocksAt (moveRockAt plot c) cs

-- calc width and height of plot
wh :: Plot -> (Int,Int)
wh plot = (w,h)
  where
    w = 1 + (maximum . map (\((x,_),_) -> x) $ plot)
    h = 1 + (maximum . map (\((_,y),_) -> y) $ plot)

weight :: Plot -> Int
weight plot = sum . map (\(_,y) -> rockWeight y) . roundRocks $ plot
  where
    (w,h) = wh plot
    rockWeight = abs . subtract h


main' contents = show ans ++ "\n"
  where
    ans = weight plot'
    plot' = moveRocksAt plot rounds
    rounds = sort . roundRocks $ plot
    plot =
        plotFromMess
      . zip [0..]
      . map charsToXCoordRocks
      . lines
      $ contents

main = interact main'
