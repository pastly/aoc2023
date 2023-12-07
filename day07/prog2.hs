import Text.Regex.Posix
import qualified Data.Set as Set
import Data.List (sort, foldl', intercalate)

type Bet = Int

data Rank =
    RJ | R2 | R3 | R4 | R5
  | R6 | R7 | R8 | R9 | RT
  | RQ | RK | RA
  deriving (Ord, Eq, Show)

toRank :: Char -> Rank
toRank '2' = R2
toRank '3' = R3
toRank '4' = R4
toRank '5' = R5
toRank '6' = R6
toRank '7' = R7
toRank '8' = R8
toRank '9' = R9
toRank 'T' = RT
toRank 'J' = RJ
toRank 'Q' = RQ
toRank 'K' = RK
toRank 'A' = RA
toRank a   = error ("Invalid rank " ++ show a)

data Hand = Hand {
    ranks :: [Rank]
  } deriving (Ord, Eq, Show)

data Strength = 
    HighCard | OnePair | TwoPair | Set
  | Boat     | Quads   | Fiver
  deriving (Ord, Eq, Show)

countRank :: Rank -> Hand -> Int
countRank r h = length . filter (\c -> c == r) . ranks $ h
countJacks = countRank RJ

bumpStrength :: Hand -> Strength
bumpStrength hand
  | nJacks == 0 = strength
  | nJacks == 1 = case strength of
      HighCard -> OnePair
      OnePair  -> Set
      TwoPair  -> Boat
      Set      -> Quads
      Boat     -> error "Impossible to have 1 J and Boat"
      Quads    -> Fiver
      Fiver    -> error "Impossible to have 1 J and Fiver"
  | nJacks == 2 && strength == OnePair = Set
  | nJacks == 2 && strength == TwoPair = Quads
  | nJacks == 2                        = Fiver
  | nJacks == 3 && strength == Set     = Quads
  | nJacks == 3 && strength == Boat    = Fiver
  | nJacks == 4                        = Fiver
  | nJacks == 5                        = Fiver
  | otherwise = error "Too many J"
  where
    nJacks = countJacks hand
    strength = handStrength hand

  

handStrength :: Hand -> Strength
handStrength h
  | length rs /= 5                     = error ("Need exactly 5 cards " ++ show h)
  | n_ranks == 1 =                       Fiver
  | n_ranks == 2 && (rs!!0) == (rs!!3) = Quads
  | n_ranks == 2 && (rs!!1) == (rs!!4) = Quads
  | n_ranks == 2 && (rs!!0) == (rs!!2) = Boat
  | n_ranks == 2                       = Boat
  | n_ranks == 3 && (rs!!0) == (rs!!2) = Set
  | n_ranks == 3 && (rs!!1) == (rs!!3) = Set
  | n_ranks == 3 && (rs!!2) == (rs!!4) = Set
  | n_ranks == 3 && (rs!!3) /= (rs!!4) = TwoPair
  | n_ranks == 3 && (rs!!0) /= (rs!!1) = TwoPair
  | n_ranks == 3                       = TwoPair
  | n_ranks == 4 && (rs!!0) == (rs!!1) = OnePair
  | n_ranks == 4 && (rs!!1) == (rs!!2) = OnePair
  | n_ranks == 4 && (rs!!2) == (rs!!3) = OnePair
  | n_ranks == 4                       = OnePair
  | otherwise =                          HighCard
  where
    n_ranks = length . Set.fromList . ranks $ h
    rs = reverse . sort . ranks $ h

parseLine :: String -> (Strength, Hand, Bet)
parseLine line = do
  let (_,_,_,(h:b:_)) = line =~ "([AKQJT2-9]+) ([0-9]+)" :: (String,String,String,[String])
  let hand = Hand . map toRank $ h
  (bumpStrength hand, hand, read b)
  

main' contents = do
  let hands = 
         sort
       . map parseLine
       . lines
       $ contents
  let answer = sum . map (\(i,(_,_,b)) -> i*b) $ zip [1..] $ hands
  let debug =
         intercalate "\n"
       . map show
       $ hands
  -- debug ++ "\n" ++ (show answer) ++ "\n"
  show answer ++ "\n"

main = interact main'
