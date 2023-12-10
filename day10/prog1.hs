import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Pipe = NS | EW | NE | NW | SW | SE | Start | Empty
  deriving (Eq, Show)
type Coord = (Int, Int)
type Plane = Map.Map Coord Pipe

toPipe :: Char -> Maybe Pipe
toPipe '|' = Just NS
toPipe '-' = Just EW
toPipe 'L' = Just NE
toPipe 'J' = Just NW
toPipe '7' = Just SW
toPipe 'F' = Just SE
toPipe 'S' = Just Start
toPipe _ = Nothing

toPipes :: [(Int, Char)] -> [(Int, Pipe)]
toPipes [] = []
toPipes ((i,c):rest) = case toPipe c of
  Nothing -> toPipes rest
  Just p -> (i,p):toPipes rest

fromMess :: [(Int, [(Int, Pipe)])] -> [(Coord, Pipe)]
fromMess [] = []
fromMess ((y,[]):rest) = fromMess rest
fromMess ((y,((x,p):xprest)):rest) = ((x,y),p):(fromMess [(y,xprest)] ++ fromMess rest)

findStart :: Plane -> Coord
findStart plane = (\(k,_) -> k) . head . filter (\(_,v) -> v == Start) . Map.assocs $ plane

hasN :: Pipe -> Bool
hasN p = elem p [NS, NE, NW, Start]
hasS :: Pipe -> Bool
hasS p = elem p [NS, SE, SW, Start]
hasE :: Pipe -> Bool
hasE p = elem p [EW, NE, SE, Start]
hasW :: Pipe -> Bool
hasW p = elem p [EW, NW, SW, Start]

connectedNeighbors :: Plane -> Int -> Int -> Coord -> [Coord]
connectedNeighbors plane width height coord = answer
  where
    answer = neighbors
    neighbors =
         take (length nPipe) nCoord
      ++ take (length sPipe) sCoord
      ++ take (length ePipe) eCoord
      ++ take (length wPipe) wCoord
    (x,y) = coord
    pipe = fromJust (Map.lookup coord plane)
    nCoord
      | hasN pipe && y-1 >= 0 = [(x,y-1)]
      | otherwise = []
    sCoord
      | hasS pipe && y+1 < height = [(x,y+1)]
      | otherwise = []
    wCoord
      | hasW pipe && x-1 >= 0 = [(x-1,y)]
      | otherwise = []
    eCoord
      | hasE pipe && x+1 < width = [(x+1,y)]
      | otherwise = []
    getOrEmpty = \c -> Map.findWithDefault Empty c plane
    nPipe = filter hasS . map getOrEmpty $ nCoord
    sPipe = filter hasN . map getOrEmpty $ sCoord
    ePipe = filter hasW . map getOrEmpty $ eCoord
    wPipe = filter hasE . map getOrEmpty $ wCoord

walk :: Plane -> Int -> Int -> [Coord] -> [Coord]
walk plane width height path
  | length nexts == 0 = path
  | otherwise = walk plane width height ((head nexts):path)
  where
    position = head path
    start = last path
    -- there should always be 0 or 1 nexts. 1 if the next step isn't the start,
    -- or 0 if it is.
    nexts =
        filter (\c -> not (elem c path))
      . connectedNeighbors plane width height
      $ position

loadPlane :: String -> Plane
loadPlane s =
    Map.fromList
  . fromMess
  . map (\(y, l) -> (y, toPipes(zip [0..] l)))
  . zip [0..]
  . lines
  $ s
  

main' contents = show answer ++ "\n"
  where
    width = length . head . lines $ contents
    height = length . lines $ contents
    plane = loadPlane contents
    start = findStart plane
    path = walk plane width height [start]
    answer = (length path) `div` 2

main = interact main'
