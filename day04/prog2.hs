import Text.Regex.Posix
import qualified Data.Set as Set
import Data.List (foldl')

parseNumbers :: String -> [Int]
parseNumbers "" = []
parseNumbers line = (\(_,_,r,m) -> (read . head $ m) : parseNumbers r) (line =~ " *([0-9]+) *" :: (String,String,String,[String]))

parseCard :: String -> ([Int], [Int])
parseCard line =
  (\(_,_,_,ms) -> (parseNumbers . head $ ms, parseNumbers . last $ ms))
  (line =~ ": ([0-9 ]+) \\| ([0-9 ]+)" :: (String,String,String,[String]))

bumpScores :: [(Int,Int)] -> Int
bumpScores [] = 0
bumpScores ((reach,reps):xs) = do
  -- recompute xs into next, where the first `reach` items are edited
  let next = 
       -- increase reps of first `reach` items
       map (\(reach',reps') -> (reach', reps'+reps)) (take reach xs)
       -- and just keep the remaining items
       ++ drop reach xs
  -- score this round is `reps` + score from all remaining rounds
  reps + bumpScores next

main' contents = do
  let a =
       bumpScores
       -- list of (reach, reps)
       . map (\a -> (length a, 1))
       . map (\(a, b) -> (Set.intersection a b))
       . map (\(a, b) -> (Set.fromList a, Set.fromList b))
       . map parseCard
       . lines
       $ contents
  show a ++ "\n"

main = interact main'
