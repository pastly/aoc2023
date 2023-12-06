import Text.Regex.Posix
import Data.List (sortBy, foldl')
import Data.Maybe (isJust)

veryBigNumber :: Integer
veryBigNumber = 100000000000

split :: String -> String -> [String]
split _ "" = []
split delim haystack = (\(a,_,rest) -> a : (split delim rest)) (haystack =~ delim :: (String,String,String))

ints :: String -> [Integer]
ints haystack = map read (getAllTextMatches (haystack =~ "[0-9]+") :: [String])

-- [1,2,3,4] --> [1,2] [2,3] [3,4]
by2' :: [a] -> [(a,a)]
by2' [] = []
by2' [_] = []
by2' (x:y:rest) = (x,y) : by2' (y:rest)

-- [1,2,3,4] --> [1,2] [3,4]
by2 :: [a] -> [(a,a)]
by2 [] = []
by2 (x:y:rest) = (x,y) : by2 rest
readSeeds :: String -> [Range]
readSeeds haystack = map (\(s,l) -> Range s (s+l))
  . by2    
  . ints
  $ haystack

-- A Range "contains" all values from `start` to `end - 1`
data Range = Range {
    start :: Integer
  , end :: Integer
  } deriving (Show)
---- Split the given range into two new ranges, where the end of the first new
---- range is one less than the given value and the start of the second new range
---- is the given value.
--splitRangeOnX :: Range -> Integer -> (Range, Range)
--splitRangeOnX r x = ((Range (start r) (x)), (Range (x) (end r)))

---- Split the first range into sub ranges based on the second range. The second
---- range must be fully inside the first range. 
--splitRangeOnRange :: Range -> Range -> [Range]
--splitRangeOnRange r r'
--  | (start r) == (start r') && (end r) == (end r') = [r]
--  | (start r) == (start r') && (end r) <  (end r') = [r,  Range (end r)  (end r')]
--  | (start r) == (start r') && (end r) >  (end r') = [r', Range (end r') (end r)]
--  | (start r) <  (start r') && (end r) == (end r') = [Range (start r) (start r'), r']
--  | (start r) >  (start r') && (end r) == (end r') = [Range (start r') (start r), r]
--  | (start r) <  (start r') && (end r) >  (end r') = [Range (start r) (start r'), r', Range (end r') (end r)]


-- If r' is partially contained with r, return the index of the intersection.
-- Otherwise return Nothing. This means you get Nothing if r' is fully inside
-- or fully outside r.
rangeIntersectsRange :: Range -> Range -> Maybe Integer
rangeIntersectsRange r r'
  | (start r') < (start r) && (end r') > (start r) = Just (start r)
  | (start r') < (end r)   && (end r') > (end r)   = Just (end r)
  | otherwise                                      = Nothing

-- If r fully contains r', return True. Otherwise return False
rangeContainsRange :: Range -> Range -> Bool
rangeContainsRange r r' = start r <= start r' && end r >= end r'
-- If r is fully inside r', return True. Otherwise return False
rangeWithinRange :: Range -> Range -> Bool
rangeWithinRange r r' = rangeContainsRange r' r

data Map = Map {
    rangesWithDiffs :: [(Range, Integer)]
  } deriving (Show)
readMap :: String -> Map
readMap string = do
  let rangesWithDiffs =
         sortBy (\(r1, _) (r2, _) -> compare (start r1) (start r2))
       . map (\[d,s,l] -> (Range s (s+l), d-s))
       . map ints
       . drop 1
       . lines
       $ string
  let (firstRange, _) = head rangesWithDiffs
  let (lastRange, _) = last rangesWithDiffs
  let rangesWithDiffs2
       | (start firstRange) > 0 = (Range 0 (start firstRange), 0) : rangesWithDiffs
       | otherwise = rangesWithDiffs
  let rangesWithDiffs3
       | (end lastRange) < veryBigNumber = rangesWithDiffs2 ++ [(Range (end lastRange) veryBigNumber, 0)]
       | otherwise = rangesWithDiffs2
  let needs =
         map (\(a, b) -> (Range (end a) (start b), 0))
       . filter (\(a, b) -> end a /= start b)
       . by2' 
       . map (\(r,_) -> r)
       $ rangesWithDiffs3
  let rangesWithDiffs4 = sortBy (\(a,_) (b,_) -> compare (start a) (start b)) (needs ++ rangesWithDiffs3)
  Map rangesWithDiffs4

--applyAll :: [a -> a] -> a -> a
--applyAll [] a = a
--applyAll (f:fs) a = applyAll fs (f a)

-- Return Nothing if range can traverse the maps. Otherwise return the int on
-- which the range must be split in order to make it further through the maps.
rangeCanTraverseMaps :: Range -> [Map] -> Maybe Integer
rangeCanTraverseMaps _ [] = Nothing
rangeCanTraverseMaps theRange (m:maps) = do
  let rwds = rangesWithDiffs m
  case (filter (\(r,_) -> (rangeWithinRange theRange r)) rwds) of
    -- No, the range crosses a range border. Figure out where and return that
    [] -> do
      let (r,_) = head . filter (\(r,_) -> isJust (rangeIntersectsRange r theRange)) $ rwds
      Just (end r)
    -- Yes the range can traverse this map, go recursive after adjusting it
    -- by the diff associated with the range it passes through.
    ((_,d):_) -> do
      let newRange = Range ((start theRange) + d) ((end theRange) + d)
      -- If a lower layer says we can't traverse, we need to adjust the index
      case (rangeCanTraverseMaps newRange maps) of
        Nothing -> Nothing
        Just i -> Just (i - d)

-- Repeatedly try to get theRange through the maps, splitting it up until all
-- pieces can make it. Return all the new ranges.
multisplitRange :: Range -> [Map] -> [Range]
multisplitRange theRange maps = do
  case (rangeCanTraverseMaps theRange maps) of
    Nothing -> [theRange]
    Just i -> multisplitRange (Range (start theRange) i) maps ++ multisplitRange (Range i (end theRange)) maps

calcDestId :: Integer -> [Map] -> Integer
calcDestId i [] = i
calcDestId i (m:ms) = do
  let (_, d) = head (filter (\(r,_) -> (start r) <= i && (end r) > i) (rangesWithDiffs m))
  calcDestId (i+d) ms

main' :: [Char] -> [Char]
main' contents = do
  let sections = split "\n\n" contents
  let seeds = readSeeds . head $ sections
  let maps =
         map readMap
       . drop 1
       $ sections
  let validRanges = foldl' (++) [] (map (\s -> multisplitRange s maps) seeds)
  let rangeMins = map (start) validRanges
  let finalMins = map (\x -> calcDestId x maps) rangeMins
  show (minimum finalMins) ++ "\n"
  

main :: IO ()
main = interact main'
