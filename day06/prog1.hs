import Text.Regex.Posix
ints :: String -> [Int]
ints haystack = map read (getAllTextMatches (haystack =~ "[0-9]+") :: [String])

countWinningTimes :: [Int] -> [Int] -> [Int]
countWinningTimes [] _ = []
countWinningTimes _ [] = []
countWinningTimes (t:ts) (d:ds) = do
  let half = div t 2
  -- calc winning times from halfway point, down
  let ws = takeWhile (\x -> x*(t-x) > d) [half, half-1..1]
  -- Double the number of winning times to account for the upward ones too.
  -- If `t` is odd, then subtract one. Doubling will double count the center
  -- time, and subtracting one will fix that.
  let n
       | even t = n' - 1
       | odd t  = n'
       where n' = 2 * (length ws)
  n : countWinningTimes ts ds

main' contents = do
  let times = ints . head . lines $ contents
  let distances = ints . last . lines $ contents
  let x = product (countWinningTimes times distances)
  show x ++ "\n"

main = interact main'
