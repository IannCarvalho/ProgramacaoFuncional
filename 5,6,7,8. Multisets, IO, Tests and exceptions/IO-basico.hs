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

import IOList
import IOMap
import qualified Data.Map as Map (fromList)

main = do
  putStrLn ""
  putStrLn "Escolha o multiconjunto implementado:"
  putStrLn "1. Lista"
  putStrLn "2. Mapa"  
  putStrLn ""
  escolha <- getLine
  opcoes
  case escolha of
    "1" -> IOList.listaMenu []
    "2" -> IOMap.mapaMenu (Map.fromList [])
    _ -> main

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
