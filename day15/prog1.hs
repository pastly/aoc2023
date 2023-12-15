import qualified Data.Text as T
import Data.Char
import Data.List

deCSV :: String -> [String]
deCSV string = map T.unpack . T.splitOn (T.pack ",") . T.pack $ string

hash :: String -> Int
hash string = foldl' (\a b -> ((a + (ord b)) * 17) `mod` 256) 0 string

-- main' contents = show (zip words hashes) ++ "\n" ++ show ans ++ "\n"
main' contents = show ans ++ "\n"
  where
    ans = sum hashes
    hashes = map hash words
    words = deCSV . init $ contents

main = interact main'
