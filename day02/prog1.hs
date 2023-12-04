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

colorLimit have colName = colorCount (head (filter (\c -> colorName c == colName) have))
isRoundPossible have round = all (\c -> colorLimit have (colorName c) >= (colorCount c)) round
isGamePossible have game = all (isRoundPossible have) (gameRounds game)
possibleGames have games = filter (isGamePossible have) games

part1 contents = do
  let parsedGames = map parseGameLine (lines contents)
  let have = [ColCount 12 "red", ColCount 13 "green", ColCount 14 "blue"]
  show (sum (map (\g -> gameId g) (possibleGames have parsedGames)))

main = interact (part1)
