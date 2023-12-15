import qualified Data.Text as T
import Data.Char
import Data.List

type Label = String
type Bucket = Int
type Op = Char
type Power = Int
type Item = (Label,Bucket,Op,Power)

type HM = [[(Label,Power)]]

deCSV :: String -> [String]
deCSV string = map T.unpack . T.splitOn (T.pack ",") . T.pack $ string

hash :: String -> Int
hash string = foldl' (\a b -> ((a + (ord b)) * 17) `mod` 256) 0 string

getLabel :: String -> Label
getLabel = takeWhile (\c -> not $ elem c "=-")

getBucket :: String -> Bucket
getBucket = hash . getLabel

getOp :: String -> Op
getOp = head . filter (\c -> elem c "=-")

getPower :: String -> Power
getPower str = case (dropWhile (\c -> elem c "=-" || isLetter c) str) of
   "" -> 0
   s -> read s

apply :: HM -> Item -> HM
apply hm (label,bucket,'-',power) = (take bucket hm) ++ [filter (\(l,p) -> l /= label) (hm!!bucket)] ++ (drop (bucket+1) hm)
apply hm (label,bucket,'=',power) = (take bucket hm) ++ [new] ++ (drop (bucket+1) hm)
  where
    old = hm!!bucket
    new = case (findIndex (\(l,p) -> l == label) old) of
      Nothing -> old ++ [(label,power)]
      Just idx -> (take idx old) ++ [(label,power)] ++ (drop (idx+1) old)

sumBox :: Int -> [(Label,Power)] -> Int
sumBox b lenses = sumBox' b (zip [1..] lenses)
  where
    sumBox' :: Int -> [(Int,(Label,Power))] -> Int
    sumBox' _ [] = 0
    sumBox' b ((s,(_,p)):rest) = (b*s*p) + sumBox' b rest

sumHM :: HM -> Int
sumHM hm = sum . map (\(i, b) -> sumBox i b) . zip [1..] $ hm

main' contents = show ans ++ "\n"
  where
    ans = sumHM hm
    hm = foldl' apply (replicate 256 []) (zip4 labels buckets ops powers)
    powers = map getPower words
    ops = map getOp words
    buckets = map getBucket words
    labels = map getLabel words
    words = deCSV . init $ contents

main = interact main'
