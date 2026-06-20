import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Ord  (comparing)

orderWeight :: [Char] -> [Char]
orderWeight = unwords . sortBy (comparing weight) . words
  where weight = sum . map digitToInt   -- New weight sorting criterion

orderLength :: [Char] -> [Char]
orderLength x = unwords $ sortBy (comparing lengthy) $ words x
  where lengthy s = length s


