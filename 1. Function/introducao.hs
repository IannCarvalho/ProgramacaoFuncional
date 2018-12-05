{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}

xor True False = True
xor False True = True
xor _ _ = False

impl a b = (not a) || b

equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}

square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}

pow x 0 = 1
pow x y | y > 0 = x * (pow x (y-1))
        | otherwise = 1/(pow x (-y))

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}

fatorial 0 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}

-- Minha Solução
isPrime x = ([] == dividers)
  where
    dividers = [y | y <- [2..(x-1)], (x `mod` y == 0)]

-- Solução do Professor
{-
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = isPrime' x [2..(x-1)]

isPrime' x [] = True
isPrime' x (y:ys) = if (mod x y) == 0 then False 
                    else isPrime' x ys
-}

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}

-- Minha Solução
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

-- Solução do Professor
{-
fib 1 = [1]	
fib 2 = [1,1]
fib x = last (fib' x)

fib' 1 = [1]
fib' 2 = [1,1]
fib' n = previous ++ [(last (init previous)) + (last previous)]
  where
    previous = fib' (n-1)
-}

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
- descricao em https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/the-euclidean-algorithm
-}

-- Minha Solução
mdc x 0 = x
mdc 0 y = y
mdc x y | x < y = mdc x (y-x)
        | otherwise = mdc (x-y) y

-- Solução do Professor
{-
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y r
  where
    r = mod x y
-}

{-
- Calcula um MMC de dois numeros. 
-}

-- Minha Solução
mmc x y = mmc' x y (max x y)

mmc' x y n  | divisivel x y n = n
            | otherwise = mmc' x y (n+1)
		   
divisivel x y n = (mod n x == 0) && (mod n y == 0)

-- Solução do Professor
{-
mmc x y = head ys
  where ys = filter (divisivel x y) [(min x y)..x*y]

divisivel x y n = (mod n x == 0) && (mod n y == 0)
-}

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}

-- Minha Solução
coprimo x y | (mdc x y) == 1 = True
      			| otherwise = False

-- Solução do Professor
coprimo x y = if (mdc x y) == 1 then True else False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [ (y,z) | y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x ]
