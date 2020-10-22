module Eval where

import AST
import Funciones
import System.IO
import System.Directory
import Data.Char
import Data.Time

eval :: FileComm -> IO ()
eval p = evalFileComm p

evalFileComm :: FileComm -> IO ()
evalFileComm (New filename comm)  = do writeFile filename "Fecha,Horario,Descripción\n"
                                       evalComm comm filename
evalFileComm (Open filename comm) = evalComm comm filename

evalComm :: Comm -> NombreArchivo -> IO ()
evalComm Skip filename = return ()
evalComm (Seq com1 com2) filename = do evalComm com1 filename
                                       evalComm com2 filename

evalComm (Insert date desc) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  date2        = printDate date
                                  time         = printTime date
                                  isEvento     = (searchEvent date2 time eventos)
                              if (isEvento == True)
                              then do putStrLn $ "Ya existe un evento el " ++ date2 ++ " a las " ++ time
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines eventos) ++ (formatEvent date2 time desc))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (InsertBetween date1 date2 desc) filename = if date1 >= date2
                                            then putStrLn "La primer fecha debe ser menor a la segunda."
                                            else agregarAux date1 date2 desc filename
evalComm (Select date) filename = do
                                content <- readFile filename
                                let linedContent   = lines content
                                    eventos        = tail linedContent
                                    eventoSelected = (verEvento (printDate date) eventos)
                                putStr (unlines eventoSelected)

-- Funciones auxiliares (no se si irían en este archivo)
agregarAux :: UTCTime -> UTCTime -> Descripcion -> NombreArchivo -> IO ()
agregarAux date1 date2 desc filename = if date1 <= date2
                                       then
                                         do evalComm (Insert date1 desc) filename
                                            agregarAux (addOneDay date1) date2 desc filename
                                       else return ()

printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date -- agrego 86400 segundos, o sea un día

formatEvent :: String -> String -> String -> String
formatEvent date time desc = date ++ "," ++ time ++ "," ++ desc ++ "\n"