{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-}

meuLast [] = error "Lista vazia!" 
meuLast [x] = x
meuLast (x:xs) = meuLast xs

{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-}

-- Minha Solução
penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo x:x2:xs | xs == [] = x2
				  | otherwise = penultimo x2:xs

-- Solução do Professor
penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo xs = last (init xs)

{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}

elementAt 1 x:xs = x
elementAt i x:xs = elementAt (i-1) xs 

{-
- Retorna o tamanho de uma lista. 
-}

meuLength [] = 0 
meuLength x:xs = 1 + meuLength xs

{-
- Retorna o inverso de uma lista. 
-}

-- Minha Solução
meuReverso [] = []
meuReverso xs = meuReverso' xs []

meuReverso' [] r = r
meuReverso' x:xs r = meuReverso' xs x:r

-- Solução do Professor
meuReverso [] = []
meuReverso (x:xs) = (meuReverso xs) ++ [x]

{-
- Diz se uma lista é palindrome. 
-}

-- Minha Solução
isPalindrome [] = True
isPalindrome xs | xs == isPalindrome' xs [] = True
				| otherwise = False
				
isPalindrome' [] r = r				
isPalindrome' x:xs r = isPalindrome' xs x:r

-- Solução do Professor
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs 
 | (head xs == last xs) = isPalindrome (init (tail xs))
 | otherwise = False
 
{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}

-- Minha Solução
compress [] = []
compress xs = compress' xs []

compress' [] r = r
compress' x:xs r | elem x r = compress' xs r
				 | otherwise = compress' xs r++[x]

-- Solução do Professor
compress [] = []
compress xs = (compress (init xs)) ++ [y | y <- [last xs], not (elem y (init xs))]

 
{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}

-- Minha Solução
compact [] = []
compact xs = compact' xs []

compact' [] r = r
compact' x:xs r = compact' (filter (!=x) xs) (r++(filter (==x) xs))

-- Solução do Professor
compact [] = []
compact xs = head (filter (== (head xs)) xs) : compact (removeList (head xs) (tail xs))

removeList e [] = []
removeList e (x:xs)
 | e == x = removeList e xs
 | otherwise = x:(removeList e xs) 

{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}

--Opção A
encode [] = []
encode xs = encode' xs []

encode' [] r = r
enconde' x:xs r = (filter (!=x) xs) (r++(x, length(filter (==x) xs)))

--Opção B
encode [] = []
encode xs = (head (ys),length ys) : encode (removeList (head xs) (tail xs))
  where
    ys = filter (== (head xs)) xs
    
{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}

-- Minha Solução
split [] = []
split xs i = split' xs i [] []

split' [] i parte r = r++parte
split' x:xs i parte r | i == 0 = split' xs i-1 [] r++parte
					  | otherwise = split' xs i-1 parte++[x] r

-- Solução do Professor
split xs i = [take i xs] ++ [drop i xs]
{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}

-- Minha Solução
slice xs imin imax = slice' xs imin-1 imax-imin []

slice' x:xs 0 0 r = r
slice' x:xs 0 imax r = slice' xs 0 imax-1 r++[x]
slice' x:xs imin imax r = slice' xs imin-1 imax-1 [] 

-- Solução do Professor
slice xs imin imax = drop (imin - 1) (take imax xs)
-- slice imin imax = (drop (imin - 1)).(take imax) 

{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}

-- Minha Solução
insertAt el pos xs = insertAt' el pos-1 xs []

insertAt' el pos [] r = r
insertAt' el 0 x:xs = r insertAt' el pos-1 x:xs r++[el]
insertAt' el pos x:xs r = insertAt' el pos-1 xs r++[x]

-- Solução do Professor
insertAt el pos xs = (take (pos-1) xs) ++ [el] ++ drop (pos-1) xs

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}

minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}

mySum xs = foldr (+) 0 xs

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}

maxList [] = undefined
maxList xs = foldr max 0 xs

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}

buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}

-- Minha Solução
media [] = 0
media xs = media' 0 0 xs

-- Solução do Professor
media' sum len [] = sum/len
media' sum len x:xs = media' sum+x len+1 xs

mean xs = (fromIntegral (sum xs))/(fromIntegral (length xs))

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}

myAppend xs ys = foldr (:) ys xs
