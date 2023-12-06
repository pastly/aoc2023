import Text.Regex.Posix

split :: String -> String -> [String]
split _ "" = []
split delim haystack = (\(a,_,rest) -> a : (split delim rest)) (haystack =~ delim :: (String,String,String))

ints :: String -> [Integer]
ints haystack = map read (getAllTextMatches (haystack =~ "[0-9]+") :: [String])

readSeeds :: String -> [Integer]
readSeeds haystack = ints haystack

data Map = Map {
    srcs :: [Integer]
  , dsts :: [Integer]
  , rngs :: [Integer] 
  } deriving (Show)
readMap :: String -> Map
readMap string = (\(a,b,c) -> Map a b c)
    . unzip3
    . map (\[d,s,j] -> (s,d,j))
    . map ints
    . drop 1
    . lines
    $ string

calcDestId :: Map -> Integer -> Integer
calcDestId map srcId = do
  let iter = zip3 (srcs map) (rngs map) (dsts map)
  case (filter (\(s,r,d) -> s <= srcId && srcId < s+r) iter) of
    [] -> srcId
    (x:xs) -> (\(s,r,d) -> srcId+d-s) x

applyAll :: [a -> a] -> a -> a
applyAll [] a = a
applyAll (f:fs) a = applyAll fs (f a)

main' contents = do
  let sections = split "\n\n" contents
  let seeds = readSeeds . head $ sections
  let calcs =
         map calcDestId
       . map readMap
       . drop 1
       $ sections
  let a = minimum . map (applyAll calcs) $ seeds
  show a ++ "\n"

main = interact main'
