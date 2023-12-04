import Data.Char (digitToInt)
import Text.Regex.Posix

findReplace search replace haystack = case haystack =~ search of
  (_, "", "") -> haystack
  (pre, search, post) -> pre ++ replace ++ findReplace search replace post

findFirstInt haystack = case haystack =~ "(one|1|two|2|three|3|four|4|five|5|six|6|seven|7|eight|8|nine|9|zero|0)" of
  "" -> error "AAAAAA"
  s -> digitToInt . head . findReplaceDigits $ s

findLastInt haystack = case (reverse haystack) =~ "(eno|1|owt|2|eerht|3|ruof|4|evif|5|xis|6|neves|7|thgie|8|enin|9|orez|0)" of
  "" -> error "AAAAA"
  s -> digitToInt . head . findReplaceDigits . reverse $ s

findReplaceDigits line =
  findReplace   "one"   "1" .
    findReplace "two"   "2" .
    findReplace "three" "3" .
    findReplace "four"  "4" .
    findReplace "five"  "5" .
    findReplace "six"   "6" .
    findReplace "seven" "7" .
    findReplace "eight" "8" .
    findReplace "nine"  "9" .
    findReplace "zero"  "0"
    $ line

sumTup :: (Int, Int) -> Int
sumTup (a, b) = a + b

part2 contents = do
  let ls = lines contents
  let firsts = map (* 10) (map findFirstInt ls)
  let lasts = map findLastInt ls
  let pairs = zip firsts lasts
  let sums = map sumTup pairs
  show (sum sums) ++ "\n"

main = do
  interact (part2)
