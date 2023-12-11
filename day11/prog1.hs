import Text.Regex.Posix

type Coord = (Int, Int)
data Grid = Grid {
    coords :: [Coord]
  } deriving (Show)

toXCoords :: String -> [Int]
toXCoords string = map (\(x,_) -> x) (getAllMatches (string =~ "#") :: [(Int,Int)])

unFoldCoords :: [([Int], Int)] -> [(Int, Int)]
unFoldCoords [] = []
unFoldCoords (([],y):rest) = unFoldCoords rest
unFoldCoords (((x:xs),y):rest) = (x,y):(unFoldCoords [(xs,y)]) ++ unFoldCoords rest

readGrid :: String -> Grid
readGrid string = Grid coords
  where
    xss = map toXCoords . lines $ string
    coords = unFoldCoords (zip xss [0..])

emptyXs :: Grid -> [Int]
emptyXs grid = out
  where
    xs = map (\(x,_) -> x) . coords $ grid
    maxX = maximum xs
    out = filter (\x -> not (x `elem` xs)) [0..maxX]

emptyYs :: Grid -> [Int]
emptyYs grid = out
  where
    ys = map (\(_,y) -> y) . coords $ grid
    maxY = maximum ys
    out = filter (\y -> not (y `elem` ys)) [0..maxY]

countBetween :: Int -> Int -> [Int] -> Int
countBetween l r ints
  | l < r = length . filter (\i -> l < i && i < r) $ ints
  | l > r = countBetween r l ints
  | otherwise = 0

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [a] = []
allPairs (a:rest) = zip (repeat a) rest ++ allPairs rest

dists :: Int -> [Int] -> [Int] -> [(Coord,Coord)] -> [Int]
dists _ _ _ [] = []
dists fact exs eys (pair:rest) = out : dists fact exs eys rest
  where
    ((x,y),(x',y')) = pair
    xDist = abs (x-x') + (fact * countBetween x x' exs)
    yDist = abs (y-y') + (fact * countBetween y y' eys)
    out = xDist + yDist
    
main' contents = "1: " ++ show answer1 ++ "\n" ++ "2: " ++ show answer2 ++ "\n"
  where
    answer1 = sum (dists 1 exs eys pairs)
    answer2 = sum (dists 999999 exs eys pairs)
    pairs = allPairs . coords $ grid
    grid = readGrid contents
    exs = emptyXs grid
    eys = emptyYs grid

main = interact main'
