import Text.Regex.Posix
import qualified Data.Set as Set
import Debug.Trace

ints :: String -> [Int]
ints haystack = map read (getAllTextMatches (haystack =~ "[-0-9]+") :: [String])

diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = []
diffs (x:y:rest) = y-x:(diffs (y:rest))

unique :: [Int] -> Int
unique xs = length . Set.fromList $ xs

predictNext :: [Int] -> Int
predictNext [] = 0
predictNext xs
  | n == 1 = prev + d
  | otherwise = prev + predictNext ds
  where
    prev = last xs
    ds = diffs xs
    d = head ds
    n = unique ds
    
    

main' contents = "Sum(Next): " ++ (show next) ++ "\nSum(Prev): " ++ (show prev) ++ "\n"
 where
   next =
       sum
     . map (predictNext . ints $)
     . lines
     $ contents
   prev =
       sum
     . map (predictNext . reverse . ints $)
     . lines
     $ contents

main = interact main'
