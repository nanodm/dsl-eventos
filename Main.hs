module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Eval
--------------------------------------

-- Las acciones de IO se ejecutan dentro de main
-- Ejemplo: runhaskell Main.hs test.txt
main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- También podemos ejecutar nuestro programa usando esta función directamente desde ghci
-- Ejemplo: run "test.txt"
run :: [Char] -> IO ()
run ifile = 
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t    -> (eval t) -- manipular el archivo
      -- Right t    -> print t  --imprimir sin evaluar (para testear Parser)

-- 1) readFile toma la ruta de un archivo y devuelve su contenido como un String
-- 2) parseComm toma los comandos proporcionados por el usuario y devuelve al programa algo con la forma del AST
-- 3) si hay un error se lo imprime por consola y si no, se llama a la función eval que ejecuta acciones de IO