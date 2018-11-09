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

import qualified MultisetList
import qualified MultisetMap


main = do
  putStrLn "Escolha o multiconjunto implementado:"
  putStrLn "1. Lista"
  putStrLn "2. Mapa\n"
  escolha <- getLine
  case escolha of
    "1" -> listaMenu []
    "2" -> mapaMenu (MultisetMap.fromList [])
    _ -> main


listaMenu lista = do
  putStrLn "\nMulticonjunto: Lista"
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
  putStrLn "13. Sair\n\n"
  opcao <- getLine
  case opcao of
    "1" -> do putStrLn "Lista: Insert"
              putStrLn "Escolha o elemento:\n"
              elemento <- getLine
              listaMenu (MultisetList.insert elemento lista)

    "2" -> do putStrLn "Lista: Remove"
              putStrLn "Escolha o elemento:\n"
              elemento <- getLine
              listaMenu (MultisetList.remove elemento lista)

    "3" -> do putStrLn "Lista: Search"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStr "Resposta: "
              putStrLn (show (MultisetList.search elemento lista))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "4" -> do putStrLn "Lista: Union"
              putStrLn "Escolha a lista:\n"
              entrada <- getLine
              let lista2 = read entrada :: Read a => [a]
              listaMenu (MultisetList.union lista lista2)

    "5" -> do putStrLn "Lista: Intersection"
              putStrLn "Escolha a lista:\n"
              entrada <- getLine
              let lista2 = read entrada :: Read a => [a]
              listaMenu (MultisetList.intersection lista lista2)

    "6" -> do putStrLn "Lista: Minus"
              putStrLn "Escolha a lista:\n"
              entrada <- getLine
              let lista2 = read entrada :: Read a => [a]
              listaMenu (MultisetList.minus lista lista2)

    "7" -> do putStrLn "Lista: Inclusion"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = read entrada :: Read a => [a]
              putStr "Resposta: "
              putStrLn (show (MultisetList.inclusion lista lista2))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "8" -> do putStrLn "Lista: Sum"
              putStrLn "Escolha a lista:\n"
              entrada <- getLine
              let lista2 = read entrada :: Read a => [a]
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
  putStrLn "\nMulticonjunto: Mapa"
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
  putStrLn "13. Sair\n\n"
  opcao <- getLine
  case opcao of
    "1" -> do putStrLn "Mapa: Insert"
              putStrLn "Escolha o elemento:\n"
              elemento <- getLine
              mapaMenu (MultisetMap.insert elemento mapa)

    "2" -> do putStrLn "Mapa: Remove"
              putStrLn "Escolha o elemento:\n"
              elemento <- getLine
              mapaMenu (MultisetMap.remove elemento mapa)

    "3" -> do putStrLn "Mapa: Search"
              putStrLn "Escolha o elemento:"
              elemento <- getLine
              putStr "Resposta: "
              putStrLn (show (MultisetMap.search elemento mapa))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              mapaMenu mapa

    "4" -> do putStrLn "Mapa: Union"
              putStrLn "Escolha o mapa:\n"
              entrada <- getLine
              let lista = read entrada :: Read a => [a]
              mapaMenu (MultisetMap.union mapa (MultisetMap.fromList lista))

    "5" -> do putStrLn "Mapa: Intersection"
              putStrLn "Escolha o mapa:\n"
              entrada <- getLine
              let lista = read entrada :: Read a => [a]
              mapaMenu (MultisetMap.intersection mapa (MultisetMap.fromList lista))

    "6" -> do putStrLn "Mapa: Minus"
              putStrLn "Escolha o mapa:\n"
              entrada <- getLine
              let lista = read entrada :: Read a => [a]
              mapaMenu (MultisetMap.minus mapa (MultisetMap.fromList lista))

    "7" -> do putStrLn "Mapa: Inclusion"
              putStrLn "Escolha o mapa:"
              entrada <- getLine
              let lista = read entrada :: Read a => [a]
              putStr "Resposta: "
              putStrLn (show (MultisetMap.inclusion mapa (MultisetMap.fromList lista)))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              mapaMenu mapa

    "8" -> do putStrLn "Mapa: Sum"
              putStrLn "Escolha o mapa:\n"
              entrada <- getLine
              let lista = read entrada :: Read a => [a]
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
