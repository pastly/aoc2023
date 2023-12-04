import Data.Char (isDigit, digitToInt)

justDigits :: [Char] -> [Char]
justDigits cs = filter isDigit cs

toInts :: [Char] -> [Int]
toInts cs = map digitToInt cs

head10AndTail :: [Int] -> [Int]
head10AndTail is = case is of
  [] -> [0, 0]
  [i] -> [i*10, i]
  is -> (head is)*10 : last is : []

part1 contents = do
  let ls = lines contents
  let ms = map justDigits ls
  let ns = map toInts ms
  let os = map head10AndTail ns
  let ps = map sum os
  show (sum ps) ++ "\n"

main = do
  interact (part1)
