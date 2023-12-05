import Text.Regex.Posix
import qualified Data.Set as Set

parseNumbers :: String -> [Int]
parseNumbers "" = []
parseNumbers line = (\(_,_,r,m) -> (read . head $ m) : parseNumbers r) (line =~ " *([0-9]+) *" :: (String,String,String,[String]))

parseCard :: String -> ([Int], [Int])
parseCard line =
  (
    parseNumbers ((\(_,_,_,m) -> head m) (line =~ ": ([0-9 ]+) \\|" :: (String,String,String,[String]))),
    parseNumbers ((\(_,_,_,m) -> head m) (line =~ "\\| ([0-9 ]+)" :: (String,String,String,[String])))
  )

score :: [a] -> Int
score [] = 0
score a = round (2 ** ((fromIntegral . length $ a) - 1))

main' contents = do
  show (sum
    . map score
    . map Set.toList
    . map (\(a, b) -> Set.intersection a b)
    . map (\(a, b) -> (Set.fromList a, Set.fromList b))
    . map parseCard
    . lines
    $ contents
    ) ++ "\n"

main = interact main'
