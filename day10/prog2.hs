import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (foldl', sort)
import Debug.Trace

data Pipe = NS | EW | NE | NW | SW | SE | Start | Empty
  deriving (Ord, Eq, Show)
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

pipeCountsAsWall :: Pipe -> Bool
pipeCountsAsWall p = hasN p

coordCountsAsWall :: Plane -> Coord -> Bool
coordCountsAsWall plane coord = pipeCountsAsWall pipe
  where
    pipe = Map.findWithDefault Empty coord plane 

nonPathCoords :: [Coord] -> Int -> Int -> [Coord]
nonPathCoords path width height = filter (\c -> not (c `elem` path)) allCoords
  where
    xs = [0..width-1]
    ys = [0..height-1]
    allCoords = foldl' (++) [] (map (\y -> zip xs (repeat y)) ys)

wallsBefore :: Plane -> [Coord] -> Coord -> Int
wallsBefore plane path (0,y) = one
  where
    one
      | coordCountsAsWall plane (0,y) = 1
      | otherwise = 0
wallsBefore plane path coord =  one + wallsBefore plane path (x-1,y)
  where
    (x,y) = coord
    one
      | coordCountsAsWall plane coord = 1
      | otherwise = 0

filterPlane :: Plane -> [Coord] -> Plane
filterPlane plane path = Map.fromList . map (\c -> (c, fromJust . Map.lookup c $ plane)) $ path

isInside :: Plane -> [Coord] -> Coord -> Bool
isInside plane path coord = ans
  where
    ans = odd nWalls
    nWalls = wallsBefore plane path coord

fixStart :: Plane -> [Coord] -> Plane
fixStart plane path = Map.insert sCoord sPipe plane
  where
    sPipe = case (aRel, sRel) of
      (( 0, _),      _) -> NS
      (( _, 0),      _) -> EW
      ((-1,-1),( 0,-1)) -> SW
      ((-1,-1),(-1, 0)) -> NE
      (( 1,-1),( 0,-1)) -> SE
      (( 1,-1),( 1, 0)) -> NW
      (( 1, 1),( 1, 0)) -> SW
      (( 1, 1),( 0, 1)) -> NE
      ((-1, 1),(-1, 0)) -> SE
      ((-1, 1),( 0, 1)) -> NW
    sCoord = last path
    aCoord = head path
    bCoord = head . drop (length path - 2) $ path
    aRel = (ax-bx, ay-by)
      where
        (ax, ay) = aCoord
        (bx, by) = bCoord
    sRel = (sx-bx, sy-by)
      where
        (sx, sy) = sCoord
        (bx, by) = bCoord


main' contents = show answer ++ "\n"
  where
    width = length . head . lines $ contents
    height = length . lines $ contents
    plane = loadPlane contents
    plane' = fixStart (filterPlane plane path) path
    start = findStart plane
    path = walk plane width height [start]
    insides = filter (isInside plane' path) (nonPathCoords path width height)
    answer = length insides

main = interact main'
