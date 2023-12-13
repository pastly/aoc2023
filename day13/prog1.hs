import qualified Data.Text as T
import Data.List
import Debug.Trace

data Plot = Plot {
    rowNs :: [Int],
    colNs :: [Int]
  } deriving (Show)

parseInt :: [Char] -> Int
parseInt [] = 0
parseInt (x:xs) = one + (2 * parseInt xs)
  where one
          | x == '.'  = 0
          | otherwise = 1

parsePlot :: String -> Plot
parsePlot string = Plot rowNs colNs
  where
    rowNs = map parseInt . lines $ string
    colNs = map parseInt . transpose . lines $ string

indexesRotatedOn :: Int -> [(Int, Int)]
indexesRotatedOn n = zip [n,n-1..0] [n+1..]

allIndexesMatch :: [(Int,Int)] -> [Int] -> Bool
allIndexesMatch [] xs = True
allIndexesMatch ((l,r):is) xs
  | r >= length xs = True
  | xs!!l /= xs!!r = False
  | otherwise      = allIndexesMatch is xs

findReflect :: [Int] -> Maybe Int
findReflect xs
  | length goodRots == 0 = Nothing
  | otherwise = Just (1 + (fst . head . head $ goodRots))
  where
    indexRots = map indexesRotatedOn [0..length xs-2]
    goodRots = filter (\rot -> allIndexesMatch rot xs) indexRots

findColReflect :: Plot -> Maybe Int
findColReflect (Plot _ xs) = findReflect xs

findRowReflect :: Plot -> Maybe Int
findRowReflect (Plot xs _) = findReflect xs

plotToAns :: Plot -> Int
plotToAns plot = case (col, row) of
  (Just i, _) -> i
  (_, Just i) -> i * 100
  where
    col = findColReflect plot
    row = findRowReflect plot

paragraphs :: String -> [String]
paragraphs string = map T.unpack . T.splitOn (T.pack "\n\n") . T.pack $ string

main' contents = show answer ++ "\n"
  where
    answer = sum . map (\para -> plotToAns . parsePlot $ para) $ paras
    paras = paragraphs contents

main = interact main'
