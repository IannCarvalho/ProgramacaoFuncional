module Util ( Util.stringToList,
              Util.sortKey,
              Util.sortValue
)
 where

import qualified Data.List as List (sortOn)

stringToList :: [Char] -> [Char] -> [(String,Integer)]
stringToList "" _ = []
stringToList string firstOne  | [h] == " " || [h] == "," || [h] == "(" || [h] == ")" = stringToList (tail string) firstOne
                              | firstOne == "0" = stringToList (tail string) [h]
                              | otherwise = [tuple] ++ (stringToList (tail string) "0") 
                                where
                                  h = head string
                                  number = read [h]
                                  tuple = (firstOne, number)

sortKey :: Ord a => [(a,b)] -> [(a,b)]
sortKey = List.sortOn fst

sortValue :: Ord b => [(a,b)] -> [(a,b)]
sortValue = List.sortOn snd