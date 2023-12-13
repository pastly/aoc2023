import qualified Data.Text as T
import Data.List
import Debug.Trace
import Data.Bits

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

calcXors :: [(Int,Int)] -> [Int] -> [Int]
calcXors [] xs = []
calcXors ((l,r):is) xs
  | r >= length xs = []
  | otherwise      = xs!!l `xor` xs!!r : calcXors is xs

findReflect :: [Int] -> Maybe Int
findReflect xs
  | length bb == 0 = Nothing
  | otherwise = Just (1 + head bb)
  where
    indexRots = map indexesRotatedOn [0..length xs-2]
    allXors = map (\rot -> calcXors rot xs) indexRots
    popCounts = map (map popCount) allXors
    isGood = (\xs -> ((length xs - 1) == (length . filter (==0) $ xs)) && (1 == (length . filter (==1) $ xs)))
    aa = map isGood popCounts
    bb = map (\(i,_) -> i) (filter (\(i, a) -> a) (zip [0..] aa))

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
