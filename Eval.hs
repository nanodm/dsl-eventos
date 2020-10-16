module Eval where

import AST
import Funciones
import System.IO
import Data.Char
import Data.Dates
import Data.Time

eval :: Comm -> IO ()
eval p = evalComm p

evalComm :: Comm -> IO ()
evalComm Skip = return ()
evalComm (Seq com1 com2) = do evalComm com1
                              evalComm com2
evalComm (Insert date desc) = agregar (printDate date) (printTime date) desc
evalComm (InsertBetween date1 date2 desc) = if date1 >= date2
                                   then putStrLn "La primer fecha debe ser menor a la segunda."
                                   else agregarAux date1 date2 desc
evalComm (Select date) = do
                        content <- readFile "archivo.csv"
                        let linedContent   = lines content
                            eventos        = tail linedContent
                            eventoSelected = (verEvento (printDate date) eventos)
                        putStrLn (unlines eventoSelected)

-- Funciones auxiliares (no se si irían en este archivo)
agregarAux :: UTCTime -> UTCTime -> String -> IO ()
agregarAux date1 date2 desc = if date1 <= date2
                              then
                                do agregar (printDate date1) (printTime date1) desc
                                   agregarAux (addOneDay date1) date2 desc
                              else return ()

printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date -- agrego 86400 segundos, o sea un día