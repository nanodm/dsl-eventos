module Eval where

import AST
import Funciones
import System.IO
import Data.Char
import Data.Dates
import Data.Time

-- eval :: Comm -> IO ()
-- eval p = evalComm p
eval :: FileComm -> IO ()
eval p = evalFileComm p

evalFileComm :: FileComm -> IO ()
evalFileComm (New filename comm) = do writeFile filename "Fecha,Horario,Descripción\n"
                                      evalComm comm filename

evalComm :: Comm -> NombreArchivo -> IO ()
evalComm Skip fn = return ()
evalComm (Seq com1 com2) fn = do evalComm com1 fn
                                 evalComm com2 fn
evalComm (Insert date desc) fn = agregar (printDate date) (printTime date) desc fn
evalComm (InsertBetween date1 date2 desc) fn = if date1 >= date2
                                            then putStrLn "La primer fecha debe ser menor a la segunda."
                                            else agregarAux date1 date2 desc fn
evalComm (Select date) fn = do
                        content <- readFile fn
                        let linedContent   = lines content
                            eventos        = tail linedContent
                            eventoSelected = (verEvento (printDate date) eventos)
                        putStrLn (unlines eventoSelected)

-- Funciones auxiliares (no se si irían en este archivo)
agregarAux :: UTCTime -> UTCTime -> String -> NombreArchivo -> IO ()
agregarAux date1 date2 desc filename = if date1 <= date2
                                       then
                                         do agregar (printDate date1) (printTime date1) desc filename
                                            agregarAux (addOneDay date1) date2 desc filename
                                       else return ()

printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date -- agrego 86400 segundos, o sea un día