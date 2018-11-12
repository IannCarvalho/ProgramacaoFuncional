module Util ( Util.stringToList,
              Util.sortKey,
              Util.sortValue
              Util.opcoes
)
 where

import qualified Data.List as List (sortOn)

opcoes = do
  putStrLn ""
  putStrLn "Escolha a função a ser testada:"
  putStrLn "1. Insert"
  putStrLn "2. Remove"
  putStrLn "3. Search"
  putStrLn "4. Union"
  putStrLn "5. Intersection"
  putStrLn "6. Minus"
  putStrLn "7. Inclusion"
  putStrLn "8. Sum"
  putStrLn "9. Size"
  putStrLn "10. Sort Key"
  putStrLn "11. Sort Value"
  putStrLn "12. Print"
  putStrLn ""

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