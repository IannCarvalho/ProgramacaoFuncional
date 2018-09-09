{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}

divTuple :: (Int, Int) -> Int
divTuple (x, 0) = undefined
divTuple (x, y) = x/y

{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}

somatorio :: Int -> Int -> Int
somatorio a b = sum [a..b]

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}

-- Minha solução

somatorioRec:: Int -> Int -> Int
somatorioRec a b = somatorioRec' a b 0

somatorioRec' :: Int -> Int -> Int -> Int
somatorioRec' a b r | a > b = r
					| otherwise = somatorioRec' a+1 b r+a

-- Solução do professor

somatorioRec :: Int -> Int -> Int
somatorioRec a b = if (a+1 == b) then a + b else a + somatorioRec (a+1) b

-- Defina a funcao que eleva um membro ao quadrado

square :: Int -> Int
square x = x*x

-- Soma os quadrados de dois numeros.

sumSquares :: Int -> Int -> Int
sumSquares x y = square x + square y

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros

higherOrderSum :: (Int -> Int) -> Int -> Int
higherOrderSum f a b = sum (map f [a,b])

-- Defina a soma dos quadrados em termos de higherOrderSum

hoSumSquares :: (Int -> Int) -> Int
hoSumSquares = higherOrderSum square

-- Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes

-- Solução do professor 1 / minha
mapFilter :: (Int -> Int) -> Int -> Int
mapFilter f p = (filter p).(map f)

-- Solução do professor 2
mapFilter :: (Int -> Int) -> Int -> [Int] -> Int
mapFilter f p xs = filter p (map f xs)
