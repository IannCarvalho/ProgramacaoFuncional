module IOList (IOList.listaMenu
)
 where

import Control.Exception as Exception
import MultisetList
import Util

data MyException = ElementoInexistente | OperacaoNaoPermitida deriving Eq
instance Exception.Exception MyException
instance Show MyException where
  show e  | e == ElementoInexistente = "O elemento é inexistente !"
          | otherwise = "A operação não é permitida !"

listaMenu lista = do
  Util.opcoes
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
              if (MultisetList.search elemento lista) == 0 then Exception.throwIO ElementoInexistente 
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
              let lista2 = Util.stringToList entrada "0"
              putStrLn ""
              listaMenu (MultisetList.union lista lista2)

    "5" -> do putStrLn "Lista: Intersection"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = Util.stringToList entrada "0"
              putStrLn ""
              listaMenu (MultisetList.intersection lista lista2)

    "6" -> do putStrLn "Lista: Minus"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = Util.stringToList entrada "0"
              putStrLn ""
              if (MultisetList.inclusion lista lista2) then Exception.throwIO OperacaoNaoPermitida
              else listaMenu (MultisetList.minus lista lista2)

    "7" -> do putStrLn "Lista: Inclusion"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = Util.stringToList entrada "0"
              putStr "Resposta: "
              putStrLn (show (MultisetList.inclusion lista lista2))
              putStrLn "Voltar (Enter)"
              voltar <- getLine
              listaMenu lista

    "8" -> do putStrLn "Lista: Sum"
              putStrLn "Escolha a lista:"
              entrada <- getLine
              let lista2 = Util.stringToList entrada "0"
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
               putStrLn (show (Util.sortKey lista))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    "11" -> do putStrLn "Lista: Sort Value"
               putStr "Lista: "
               putStrLn (show (Util.sortValue lista))
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    "12" -> do putStrLn "Lista: Print"
               putStr "Lista: "
               putStrLn (show lista)
               putStrLn "Voltar (Enter)"
               voltar <- getLine
               listaMenu lista

    _ -> listaMenu lista