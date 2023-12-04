import Data.Char (digitToInt)
import Text.Regex.Posix

data Game = Game Int [[ColCount]]
  deriving (Show)
type ColName = String
data ColCount = ColCount Int ColName
  deriving (Show)
gameId (Game id _) = id
gameRounds (Game _ rounds) = rounds
colorCount (ColCount cnt _) = cnt
colorName (ColCount _ name) = name


parseCol [cntStr, name] = ColCount (read cntStr) name

parseGameData line = do
  let (_, _, rest, thisCol) = line =~ " *([0-9]+) ([a-z]+),? *" :: (String, String, String, [String])
  case rest of
    "" -> [parseCol thisCol]
    _ -> [parseCol thisCol] ++ parseGameData rest

parseGameDatas line = do
  let (unmatchedLead, _, rest, thisGames) = line =~ " *([^;]+); *" :: (String, String, String, [String])
  case unmatchedLead of
    "" -> [parseGameData (head thisGames)] ++ parseGameDatas rest
    _ -> [parseGameData unmatchedLead]

parseGameLine line = do
  let (_, _, rest, gameIds) = line =~ "Game ([0-9]+): " :: (String, String, String, [String])
  let gameId = read (head gameIds) :: Int
  let g = Game gameId (parseGameDatas rest)
  g

findColorCount colName round = do
  case filter (\cc -> colorName cc == colName) round of
    [] -> 0
    [cc] -> colorCount cc

gameProduct game = do
  let r = maximum (map (findColorCount "red") (gameRounds game))
  let g = maximum (map (findColorCount "green") (gameRounds game))
  let b = maximum (map (findColorCount "blue") (gameRounds game))
  r * g * b

part2 contents = do
  let parsedGames = map parseGameLine (lines contents)
  let a = map gameProduct parsedGames
  show (sum a) ++ "\n"

main = interact (part2)
