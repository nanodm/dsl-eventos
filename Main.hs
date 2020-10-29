module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Eval
--------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = 
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t    -> (eval t) -- manipular el archivo
      -- Right t    -> print t  --imprimir sin evaluar (para testear Parser)