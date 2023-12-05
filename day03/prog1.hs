import Text.Regex.Posix
import Data.Foldable
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

-- y, (x, len)
type NumPos = (Int, (Int, Int))
-- (x, y)
type Coord = (Int, Int)

-- parseEngine lines = do
--   let linesWithY = zip [0..] lines

calcPartCoords (y, line) =
  zip
    (map fst (getAllMatches (line =~ "[^0-9.]") :: [(Int, Int)]))
    (repeat y)
  
calcNumPos (y, line) =
  zip
    (repeat y)
    (getAllMatches (line =~ "[0-9]+") :: [(Int, Int)])

numAtPos :: [String] -> NumPos -> Int
numAtPos lines (y,(x,len)) = read (take len (drop x (lines!!y)))

doesNumCover :: NumPos -> Coord -> Bool
doesNumCover (n_y, (n_x, n_len)) (x, y)
  | n_y /= y = False
  | n_x + n_len - 1 < x = False
  | n_x > x = False
  | otherwise = True

coordsToCheck :: (Int, Int) -> Coord -> [Coord]
coordsToCheck (w, h) (cx, cy) = do
  let poss = [
       (cx-1, cy-1), (cx, cy-1), (cx+1, cy-1),
       (cx-1, cy  ),             (cx+1, cy  ),
       (cx-1, cy+1), (cx, cy+1), (cx+1, cy+1)]
  -- I was ready to filter out impossible coords (due to being off the grid,
  -- e.g. because less than zero), but a peek at both the test input data and
  -- real input data reveal the inputs are crafted to avoid that problem.
  -- Thanks I guess.
  poss

-- coordsToCheck :: (Int, Int) -> NumPos -> [Coord]
-- coordsToCheck (w, h) (y, (x, len)) = do
--   let poss =
--        zip [x-1..x+len] (repeat (y-1)) ++
--        [(x-1, y), (x+len, y)] ++
--        zip [x-1..x+len] (repeat (y+1))
--   filter (\(x, y) -> x >= 0 && x < w && y >= 0 && y < h) poss

checkCoord :: [NumPos] -> Coord -> Maybe [NumPos]
checkCoord numPoss coord =
  case filter (\n -> doesNumCover n coord) numPoss of
    [] -> Nothing
    n -> Just n

main' contents = do
  let lines' = lines contents
  let width = length (head lines')
  let height = length lines'
  -- parts are (x,y) where (0,0) is top left
  let partCoords = fold (map calcPartCoords (zip [0..] lines'))
  let numPoss = fold (map calcNumPos (zip [0..] lines'))
  let checkCoords = fold . map (coordsToCheck (width, height)) $ partCoords
  let a = sum
       . map (numAtPos lines')
       . Set.toList
       . Set.fromList
       . fold
       . catMaybes
       . map (checkCoord numPoss)
       $ checkCoords
  show a ++ "\n"
  

main = interact main'
