import Data.Text (singleton, pack, unpack, splitOn)
import Data.List (foldl')
import Text.Regex.Posix
import Debug.Trace

data State = G | B | U
  deriving (Eq, Show)

charsToStates :: [Char] -> [State]
charsToStates [] = []
charsToStates ('.':cs) = G : charsToStates cs
charsToStates ('#':cs) = B : charsToStates cs
charsToStates ('?':cs) = U : charsToStates cs

ints :: String -> [Int]
ints haystack = map read (getAllTextMatches (haystack =~ "[-0-9]+") :: [String])

-- Return all the ways that you can split m into n pieces, each >1, where the
-- sum of the pieces is m. Order matters (e.g. the arrangements [1,1,2] and
-- [2,1,1] are unique)
arrangements :: Int -> Int -> [[Int]]
arrangements m 1 = [[m]]
arrangements m n = filter (\is -> not (0 `elem` (take (length is - 2) . drop 1 $ is))) aaaa
  where
    firsts = [m-n+2,m-n+1..0]
    rems = map (m-) firsts
    aaaa = foldl' (++) [] (map (\(f, r) -> map (\a -> f:a) (arrangements r (n-1))) (zip firsts rems))

alternate :: [a] -> [a] -> [a]
alternate []     []     = []
alternate [a]    []     = [a]
alternate []     [b]    = [b]
alternate (a:as) (b:bs) = a:b:alternate as bs

fits :: [State] -> [State] -> Bool
fits [] [] = True
fits (t:ts) (a:as)
  | t == a || t == U = fits ts as
  | otherwise = False

try :: [State] -> [Int] -> [Int] -> Bool
try truth bads goods = answer
  where
    answer = fits truth attempt
    bads' =  map (\n -> take n . repeat $ B)  bads
    goods' = map (\n -> take n . repeat $ G) goods
    attempt = foldl' (++) [] (alternate goods' bads')


doLine :: ([State], [Int]) -> Int
doLine (truth, bads) = n
  where
    n = length . filter (==True) . map (try truth bads) $ goodArrs
    goodArrs = arrangements nGoods (nGaps+2)
    nGaps = length bads - 1
    nGoods = length truth - sum bads

main' contents = show answer ++ "\n"
  where
    answer =
        sum
      . map doLine
      . map (\[a,b] -> (charsToStates . unpack $ a, ints . unpack $ b))
      . map (splitOn (singleton ' '))
      . map pack
      . lines
      $ contents

main = interact main'
