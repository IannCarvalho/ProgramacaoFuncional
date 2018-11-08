module MultisetMap ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import Data.Map as Map

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem bag | member elem bag = Map.insert elem plusOne bag
                | otherwise = Map.insert elem 1 bag
                  where
                    plusOne = (bag ! elem) + 1

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}

remove elem bag | lessOne > 0 = Map.insert elem lessOne bag
                | otherwise = Map.delete elem bag
                  where
                    lessOne = (bag ! elem) - 1

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem bag | member elem bag = (bag ! elem)
                | otherwise = 0

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}

union bag1 (fromList []) = bag1
union (fromList []) bag2 = bag2
union bag1 bag2 | qnt1 < qnt2 = union (Map.insert elem qnt2 bag1) t2
                | otherwise = union bag1 t2
                  where
                    list2 = toList (bag2)
                    elem = fst (head (list2))
                    qnt1 = search elem bag1
                    qnt2 = snd (head (list2))
                    t2 = (fromList (tail list2))
{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection bag1 (fromList []) = bag1
intersection (fromList []) bag2 = bag2
intersection bag1 bag2 = intersection' bag1 bag2 (fromList [])

intersection' bag1 (fromList []) result = result
intersection' bag1 bag2 result| qnt1 == 0 = intersection' bag1 t2 result
                              | qnt1 > qnt2 = intersection' bag1 t2 (Map.insert elem qnt2 result)
                              | otherwise = intersection' bag1 t2 (Map.insert elem qnt1 result)
                                where
                                  list2 = toList (bag2)
                                  elem = fst (head (list2))
                                  qnt1 = search elem bag1
                                  qnt2 = snd (head (list2))
                                  t2 = (fromList (tail list2))

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}

minus (fromList []) bag2 = 
minus bag1 bag2 | dif > 0 = Map.insert elem1 dif (minus t1 bag2)
                | otherwise = minus t1 bag2  
                  where
                    list1 = toList bag1
                    elem1 = fst (head list1)
                    quant1 = snd (head list1)
                    quant2 = search elem1 bag2
                    dif = quant1 - quant2
                    t1 = (fromList (tail list1))


{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}

inclusion bag1 (fromList []) = True
inclusion bag1 bag2 | member elem bag1 = inclusion bag1 t2
                    | otherwise = False
                      where 
                        list2 = toList bag2
                        elem = fst (head list2)
                        t2 = (fromList (tail list2))

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum bag1 bag2 = Map.unionWith (+) bag1 bag2

{-
 - Retorna a quantidade total de elementos no Bag
-}
size bag = Prelude.sum (elems bag)