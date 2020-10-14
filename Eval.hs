module Eval where

import AST
import Funciones
import System.IO
import Data.Char
import Data.Dates
import Data.Time

evalComm :: AddComm -> IO ()
evalComm (Add1 date desc) = agregar (printDate date) (printTime date) desc
evalComm (Add2 date1 date2 desc) = agregarAux date1 date2 desc

agregarAux :: UTCTime -> UTCTime -> String -> IO ()
agregarAux date1 date2 desc = if date1 <= date2
                              then
                                do agregar (printDate date1) (printTime date1) desc
                                   agregarAux (funcionTest date1) date2 desc
                              else return ()
