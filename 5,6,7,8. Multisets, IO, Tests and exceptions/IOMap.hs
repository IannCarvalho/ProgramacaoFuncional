module IOMap (IOMap.mapaMenu
)
 where

import Control.Exception as Exception
import MultisetMap
import MultisetList
import Util

data MyException = ElementoInexistente | OperacaoNaoPermitida deriving Eq
instance Exception.Exception MyException
instance Show MyException where
  show e  | e == ElementoInexistente = "O elemento é inexistente !"
          | otherwise = "A operação não é permitida !"

mapaMenu mapa = do
    opcao <- getLine
    putStrLn ""
    case opcao of
      "1" -> do putStrLn "Mapa: Insert"
                putStrLn "Escolha o elemento:"
                elemento <- getLine
                putStrLn ""
                mapaMenu (MultisetMap.insert elemento mapa)
      
      "2" -> do putStrLn "Mapa: Remove"
                putStrLn "Escolha o elemento:"
                elemento <- getLine
                putStrLn ""
                if (MultisetMap.search elemento mapa) == 0 then Exception.throwIO ElementoInexistente 
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
                let lista = Util.stringToList entrada "0"
                print lista
                putStrLn ""
                mapaMenu (MultisetMap.union mapa (MultisetMap.fromList lista))
  
      "5" -> do putStrLn "Mapa: Intersection"
                putStrLn "Escolha o mapa:"
                entrada <- getLine
                let lista = Util.stringToList entrada "0"
                putStrLn ""
                mapaMenu (MultisetMap.intersection mapa (MultisetMap.fromList lista))
  
      "6" -> do putStrLn "Mapa: Minus"
                putStrLn "Escolha o mapa:"
                entrada <- getLine
                let lista = Util.stringToList entrada "0"
                putStrLn ""
                if (MultisetMap.inclusion mapa (MultisetMap.fromList lista)) then Exception.throwIO OperacaoNaoPermitida
                else mapaMenu (MultisetMap.minus mapa (MultisetMap.fromList lista))
                
  
      "7" -> do putStrLn "Mapa: Inclusion"
                putStrLn "Escolha o mapa:"
                entrada <- getLine
                let lista = Util.stringToList entrada "0"
                putStr "Resposta: "
                putStrLn (show (MultisetMap.inclusion mapa (MultisetMap.fromList lista)))
                putStrLn "Voltar (Enter)"
                voltar <- getLine
                mapaMenu mapa
  
      "8" -> do putStrLn "Mapa: Sum"
                putStrLn "Escolha o mapa:"
                entrada <- getLine
                let lista = Util.stringToList entrada "0"
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
                 putStrLn (show (Util.sortKey (MultisetMap.toList mapa)))
                 putStrLn "Voltar (Enter)"
                 voltar <- getLine
                 mapaMenu mapa
  
      "11" -> do putStrLn "Mapa: Sort Value"
                 putStr "Mapa: "
                 putStrLn (show (Util.sortValue (MultisetMap.toList mapa)))
                 putStrLn "Voltar (Enter)"
                 voltar <- getLine
                 mapaMenu mapa
  
      "12" -> do putStrLn "Mapa: Print"
                 putStr "Mapa: "
                 putStrLn (show mapa)
                 putStrLn "Voltar (Enter)"
                 voltar <- getLine
                 mapaMenu mapa
  
      _ -> mapaMenu mapa