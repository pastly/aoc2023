import Text.Regex.Posix

data Dir = L | R
  deriving (Eq, Show)

parseDirs :: String -> [Dir]
parseDirs [] = []
parseDirs (d:ds)
  | d == 'L' = L : parseDirs ds
  | d == 'R' = R : parseDirs ds
  | otherwise = error ("Invalid Dir: " ++ show d)

type Tree = [(Node, (Node, Node))]
type Node = String
parseLines :: [String] -> [(Node, (Node, Node))]
parseLines [] = []
parseLines (line:ls) = do
  let [n,l,r] = getAllTextMatches (line =~ "[A-Z]+") :: [String]
  (n,(l,r)) : parseLines ls

travel :: Tree -> [Dir] -> Node -> Node -> Int
travel tree dirs end start
  | end == start = 0
  | d == L       = 1 + travel tree dirs' end l
  | d == R       = 1 + travel tree dirs' end r
  | otherwise    = error "Cannot find path"
  where
    (d:dirs') = dirs
    (l, r) = (\(_,lr) -> lr) . head . filter (\(n,_) -> start==n) $ tree


main' content = do
  let dirs = cycle . parseDirs . head . lines $ content
  let tree = parseLines . drop 2 . lines $ content
  let answer = travel tree dirs "ZZZ" "AAA"
  show answer ++ "\n"

main = interact main'
