--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
-- pow x y = undefined
pow = \x y -> if (y==1) then x else x * pow x (y-1) 
-- fatorial x = undefined
fatorial = \x -> if (x==1) then 1 else x * fatorial (x-1)
-- isPrime x = undefined
isPrime = \x -> if (x==1) then True else isPrime' x (x-1)
isPrime' = \x y -> if (y==1) then True else ((x `mod` y ) /= 0) && isPrime' x (y-1)
-- fib x = undefined
fib = \x -> if (x==0) then 1 else fib(x-1) + fib(x-2)
-- mdc x y = undefined
mdc = \x y -> if (y==0) then x else mdc y (x `mod` y)
-- mmc x y = undefined
mmc = \x y -> (x * (y `div` (mdc x y)))
-- coprimo x y = undefined
coprimo = \x y -> (mdc x y) == 1
-- goldbach x = undefined
goldbach = \x -> [(a, b) | a <- filter isPrime [1..x], b <- filter isPrime [1..x], a + b == x]

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

-- meuLast xs = undefined
meuLast = \xs -> if (length xs == 0) then error "Lista Vazia!" else if (length xs == 1) then head xs else meuLast (tail xs)
-- penultimo xs = undefined
penultimo = \xs -> if (length xs < 2) then error "Lista sem penultimo" else if (length xs == 2) then head xs else penultimo (tail xs)
-- elementAt i xs = undefined
elementAt = \i xs -> if (i == 1) then head xs else elementAt (i-1) (tail xs)
-- meuLength xs = undefined
meuLength = \xs -> sum [1 | x <- xs]
-- meuReverso xs = undefined
meuReverso = \xs -> if (length xs == 0) then [] else meuReverso (tail xs) ++ [head xs]
-- isPalindrome xs = undefined
isPalindrome = \xs -> xs == meuReverso xs
-- compress xs = undefined
compress = \xs -> if (length xs == 0) then [] else if (elem (head xs) (tail xs)) then compress (tail xs) else [head xs] ++ compress (tail xs)
-- compact xs = undefined
compact = \xs -> if (length xs == 0) then [] else (compress (init xs)) ++ [y | y <- [last xs], not (elem y (init xs))]
-- encode xs = undefined
encode = \xs -> if (length xs == 0) then [] else (head (ys),length ys) : encode (removeList (head xs) (tail xs)) where ys = filter (== (head xs)) xs
-- split xs i = undefined
split = \xs i -> [take i xs] ++ [drop i xs]
-- slice xs imin imax = undefined
slice = \xs imin imax -> drop (imin - 1) (take imax xs)
-- insertAt el pos xs = undefined
insertAt = \el pos xs -> (take (pos-1) xs) ++ [el] ++ drop (pos-1) xs
-- sort xs = undefined
sort = \xs -> if (length xs == 0) then [] else sort xs = x:ys where x = (minList xs) ys = sort (remove x xs) 
-- mySum xs = undefined
mySum = \xs -> foldr (+) 0 xs
-- maxList xs = undefined
maxList = \xs -> if (length xs == 0) then undefined else foldr max 0 xs
-- buildPalindrome xs = undefined
buildPalindrome = \xs -> if (length xs == 0) then [] else [head xs] ++ buildPalindrome xs ++ [head xs]
-- mean xs = undefined
mean = \xs -> (sum xs) `div` (length xs)
-- myAppend xs ys = undefined
myAppend = \xs ys -> foldr (:) ys xs
