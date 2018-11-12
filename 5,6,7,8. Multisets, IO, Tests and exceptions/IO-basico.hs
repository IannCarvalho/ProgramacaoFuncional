-- IO

{-
Neste exercicio voce vai implementar um menu que permite o usuario escolher
a sua versao de multiconjunto implementada (multiset) com lista ou map.

Primeiramente o menu deve oferecer escolha de que implementação testar. depois ele
deve oferecer opcoes de invocar cada funcao presente no multiset e executa-la
quando escolhida.

Duas funcionalidades devem sempre estar presentes para cada estrutura: a impressão da estrutura em si
para que ela seja visualizada, e uma funcao que ordena os elementos da estrutura por chave/valor ou pela
quantidade de ocorrencias. No caso da função de ordenação ser invocada, ela deve imprimir a estrutura
apos a ordenação.
-}

-- Exceptions

{- 
- Remove: não é possivel remover um element que nao existe no bag. Neste caso,
- deve-se retornar uma exceção chamada ElementoInexistente
- Minus: na subtração de bags, caso um elemento x esteja no primeiro bag com
- uma quantidade menor que no bag a ser subtraído, então deve retornar uma
- excecao OperacaoNaoPermitida. Exemplo: {(a,3)} \ {(a,4)} retornaria essa
- exceção porque o primeiro Bag nao possui quantidade suficiente de ‘a’ para
- subtrair.
- Embora as excecoes acima sejam tipificadas, é sempre bom ter uma mensagem
- de texto associada a elas. Considere isso na hora que for implementa-las.
-}

import qualified MultisetList
import qualified MultisetMap
import Control.Exception as Exception


data MyException = ElementoInexistente | OperacaoNaoPermitida deriving Eq
instance CE.Exception MyException
instance Show MyException where
  show e  | e == ElementoInexistente = "O elemento é inexistente !"
          | otherwise = "A operação não é permitida !"

main = do
  putStrLn ""
  putStrLn "Escolha o multiconjunto implementado:"
  putStrLn "1. Lista"
  putStrLn "2. Mapa"  
  putStrLn ""
  escolha <- getLine
  case escolha of
    "1" -> listaMenu []
    "2" -> mapaMenu (MultisetMap.fromList [])
    _ -> main

listaMenu lista = do
  opcoes
  opcao <- getLine
  putStrLn ""
  case opcao of
    "1" -> do putStrLn "Lista: Insert"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStrLn ""
              listaMenu (MultisetList.insert elemento lista)

    "2" -> do putStrLn "Lista: Remove"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStrLn ""
              if (MultisetList.search elemento lista) == 0 then CE.throwIO ElementoInexistente 
              else listaMenu (MultisetList.remove elemento lista)

    "3" -> do putStrLn "Lista: Search"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStr "Resposta: "
              putStrLn (show (MultisetList.search elemento lista))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "4" -> do putStrLn "Lista: Union"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = stringToList entrada "0"
              putStrLn ""
              listaMenu (MultisetList.union lista lista2)

    "5" -> do putStrLn "Lista: Intersection"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = stringToList entrada "0"
              putStrLn ""
              listaMenu (MultisetList.intersection lista lista2)

    "6" -> do putStrLn "Lista: Minus"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = stringToList entrada "0"
              putStrLn ""
              if (MultisetList.inclusion mapa (MultisetList.fromList lista)) then CE.throwIO OperacaoNaoPermitida
              else listaMenu (MultisetList.minus mapa (MultisetList.fromList lista))

    "7" -> do putStrLn "Lista: Inclusion"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = stringToList entrada "0"
              putStr "Resposta: "
              putStrLn (show (MultisetList.inclusion lista lista2))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "8" -> do putStrLn "Lista: Sum"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = stringToList entrada "0"
              putStrLn ""
              listaMenu (MultisetList.sum lista lista2)

    "9" -> do putStrLn "Lista: Size"
              putStr "Resultado: "
              putStrLn (show (MultisetList.size lista))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "10" -> do putStrLn "Lista: Sort Key"
               putStr "Lista: "
               putStrLn (show (MultisetList.sortKey lista))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    "11" -> do putStrLn "Lista: Sort Value"
               putStr "Lista: "
               putStrLn (show (MultisetList.sortValue lista))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    "12" -> do putStrLn "Lista: Print"
               putStr "Lista: "
               putStrLn (show lista)
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    "13" -> main

    _ -> listaMenu lista

mapaMenu mapa = do
  opcoes
  opcao <- getLine
  putStrLn ""
  case opcao of
    "1" -> do putStrLn "Mapa: Insert"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStrLn ""
              listaMenu (MultisetMap.insert elemento mapa)
    
    "2" -> do putStrLn "Mapa: Remove"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStrLn ""
              if (MultisetMap.search elemento lista) == 0 then CE.throwIO ElementoInexistente 
              else mapaMenu (MultisetMap.remove elemento mapa)
              
    "3" -> do putStrLn "Mapa: Search"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStr "Resposta: "
              putStrLn (show (MultisetMap.search elemento mapa))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              mapaMenu mapa

    "4" -> do putStrLn "Mapa: Union"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = stringToList entrada "0"
              print lista
              putStrLn ""
              mapaMenu (MultisetMap.union mapa (MultisetMap.fromList lista))

    "5" -> do putStrLn "Mapa: Intersection"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = stringToList entrada "0"
              putStrLn ""
              mapaMenu (MultisetMap.intersection mapa (MultisetMap.fromList lista))

    "6" -> do putStrLn "Mapa: Minus"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = stringToList entrada "0"
              putStrLn ""
              if (MultisetList.inclusion mapa (MultisetList.fromList lista)) then CE.throwIO OperacaoNaoPermitida
              else mapaMenu (MultisetMap.minus mapa (MultisetMap.fromList lista))
              

    "7" -> do putStrLn "Mapa: Inclusion"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = stringToList entrada "0"
              putStr "Resposta: "
              putStrLn (show (MultisetMap.inclusion mapa (MultisetMap.fromList lista)))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              mapaMenu mapa

    "8" -> do putStrLn "Mapa: Sum"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = stringToList entrada "0"
              putStrLn ""
              mapaMenu (MultisetMap.sum mapa (MultisetMap.fromList lista))

    "9" -> do putStrLn "Mapa: Size"
              putStr "Resultado: "
              putStrLn (show (MultisetMap.size mapa))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              mapaMenu mapa

    "10" -> do putStrLn "Mapa: Sort Key"
               putStr "Mapa: "
               putStrLn (show (MultisetList.sortKey (MultisetMap.toList mapa)))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               mapaMenu mapa

    "11" -> do putStrLn "Mapa: Sort Value"
               putStr "Mapa: "
               putStrLn (show (MultisetList.sortValue (MultisetMap.toList mapa)))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               mapaMenu mapa

    "12" -> do putStrLn "Mapa: Print"
               putStr "Mapa: "
               putStrLn (show mapa)
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               mapaMenu mapa

    "13" -> main

    _ -> mapaMenu mapa

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
  putStrLn "13. Sair"
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