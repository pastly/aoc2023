import Data.Text (singleton, pack, unpack, splitOn)
import Data.List (foldl')
import Text.Regex.Posix
import Debug.Trace
import Data.Function.Memoize
import Language.Haskell.TH

data State = G | B | U
  deriving (Eq, Show)

deriveMemoizable ''State

charsToStates :: [Char] -> [State]
charsToStates [] = []
charsToStates ('.':cs) = G : charsToStates cs
charsToStates ('#':cs) = B : charsToStates cs
charsToStates ('?':cs) = U : charsToStates cs

ints :: String -> [Int]
ints haystack = map read (getAllTextMatches (haystack =~ "[-0-9]+") :: [String])

fits :: [State] -> [State] -> Bool
fits [] [] = True
fits (t:ts) (a:as)
  | t == a || t == U = fits ts as
  | otherwise = False

badsToPieces :: [Int] -> [[State]]
badsToPieces [] = []
badsToPieces [i] = (take i . repeat $ B) : []
badsToPieces (i:is) = ((take i . repeat $ B)++[G]) : badsToPieces is

stripRepeatedGoods :: [State] -> [State]
stripRepeatedGoods [] = []
stripRepeatedGoods (B:xs)   = B : stripRepeatedGoods xs
stripRepeatedGoods (U:xs)   = U : stripRepeatedGoods xs
stripRepeatedGoods [G]      = G : []
stripRepeatedGoods (G:G:xs) =     stripRepeatedGoods (G:xs)
stripRepeatedGoods (G:x:xs) = G : stripRepeatedGoods (x:xs)

allPiecesFit :: [State] -> [[State]] -> Bool
allPiecesFit [] [] = True
allPiecesFit [] pieces = False -- no b/c pieces left after end of truth
allPiecesFit truth [] = [] == filter (==B) truth -- yes if none of remaining truth is a bad piece (only good or unknown)
allPiecesFit (G:ts) pieces = fastAPF ts pieces
allPiecesFit (U:ts) pieces = fastAPF (G:ts) pieces || fastAPF (B:ts) pieces
allPiecesFit truth (piece:pieces)
  | length piece > length truth = False -- not enough truth left to even fit in the next piece
  | not firstPieceFits = False -- next piece doesnt match up with the truth
  | otherwise = fastAPF (drop (length piece) truth) pieces -- yes, but go recursive to see if the rest does
  where
    pairs = zip truth piece
    firstPieceFits = all (==True) . map (\(t, p) -> t == p || t == U) $ pairs

fastAPF = memoize2 allPiecesFit

countFittings :: [State] -> [[State]] -> Int
countFittings [] _ = 0
countFittings (U:ts) pieces = fastCF (G:ts) pieces + fastCF (B:ts) pieces
countFittings (G:ts) pieces = fastCF ts pieces
countFittings (B:ts) (p:ps) = one * variations
  where
    nextTruth = drop (length p) (B:ts)
    variations
      | length ps == 0 = 1
      | otherwise = fastCF nextTruth ps
    one
      | fastAPF (B:ts) (p:ps) = 1
      | otherwise = 0

fastCF = memoize2 countFittings

main' contents = show ans ++ "\n"
  where
    ans =
        sum
      . map (\(a,b) -> fastCF a b)
      . map (\[a,b] -> (charsToStates . unpack $ a, badsToPieces . ints . unpack $ b))
      . map (splitOn (singleton ' '))
      . map pack
      . lines
      $ contents

main = interact main'
