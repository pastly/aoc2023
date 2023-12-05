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
    (map fst (getAllMatches (line =~ "\\*") :: [(Int, Int)]))
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

coordsToCheck :: Coord -> [Coord]
coordsToCheck (cx, cy) = do
  let poss = [
       (cx-1, cy-1), (cx, cy-1), (cx+1, cy-1),
       (cx-1, cy  ),             (cx+1, cy  ),
       (cx-1, cy+1), (cx, cy+1), (cx+1, cy+1)]
  -- I was ready to filter out impossible coords (due to being off the grid,
  -- e.g. because less than zero), but a peek at both the test input data and
  -- real input data reveal the inputs are crafted to avoid that problem.
  -- Thanks I guess.
  poss

partHasPair :: [String] -> [NumPos] -> Coord -> Maybe [Int]
partHasPair lines' numPoss coord = do
  let coords = coordsToCheck coord
  let checks = [(np, c) | np <- numPoss, c <- coords]
  let a = Set.toList
        . Set.fromList
        . map (numAtPos lines')
        . map (\(np, _) -> np)
        . filter (\(np, c) -> doesNumCover np c)
        $ checks
  case length a of
    2 -> Just a
    _ -> Nothing

main' contents = do
  let lines' = lines contents
  let numPoss = fold (map calcNumPos (zip [0..] lines'))
  let partCoords = fold (map calcPartCoords (zip [0..] lines'))
  let a = sum
        . map product
        . catMaybes
        . map (partHasPair lines' numPoss)
        $ partCoords
  show a ++ "\n"

main = interact main'
