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

evalComm (InsertWeekly date1 date2 desc dia) filename = do 
                                                          let day = getWeekDay dia
                                                              firstDay = getDateByWeekDay date1 day
                                                          do agregarAux3 firstDay date2 desc filename --putStrLn $ show firstDay

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
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines eventos) ++ (formatEvent date2 time desc))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (InsertBetween date1 date2 desc) filename = if date1 > date2
                                            then putStrLn "La primer fecha debe ser menor a la segunda."
                                            else agregarAux date1 date2 desc filename
evalComm (InsertAllDays date1 date2 desc) filename = if date1 < date2
                                                     then agregarAux2 date1 date2 desc filename
                                                     else return ()

evalComm (SelectDate date) filename = do
                                content <- readFile filename
                                let linedContent   = lines content
                                    eventos        = tail linedContent
                                    eventoSelected = (verEvento (printDate date) eventos)
                                putStr (unlines eventoSelected)

evalComm (SelectFullDate date) filename = do
                                content <- readFile filename
                                let linedContent   = lines content
                                    eventos        = tail linedContent
                                    eventoSelected = (verEvento2 (printDate date) (printTime date) eventos)
                                putStr (unlines eventoSelected)

evalComm (UpdateDescription date desc) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (modificarDescripcionEvento (printDate date) (printTime date) desc eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (UpdateFullDate date newDate) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (modificarFechaEvento (printDate date) (printTime date) (printDate newDate) (printTime newDate) eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (UpdateDate date newDate) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (modificarFechaEvento2 (printDate date) (printDate newDate) eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (UpdateTime date newDate) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (modificarFechaEvento3 (printDate date) (printTime date) (printTime newDate) eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (CancelEventDate date) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (cancelarEvento2 (printDate date) (printTime date) eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para cancelar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename

evalComm (CancelEventDay date) filename = do
                              handle <- openFile filename ReadMode
                              (tempName, tempHandle) <- openTempFile "." "temp"
                              content <- hGetContents handle
                              let linedContent = lines content
                                  header       = head linedContent
                                  eventos      = tail linedContent
                                  result       = (cancelarEvento (printDate date) eventos)
                              if (eventos == result)
                              then do putStrLn $ "No hay eventos para cancelar el " ++ (printDate date)
                                      hClose handle
                                      hClose tempHandle
                                      removeFile tempName
                              else do
                                   hPutStr tempHandle $ (header ++ "\n" ++ (unlines result))
                                   hClose handle
                                   hClose tempHandle
                                   removeFile filename
                                   renameFile tempName filename


-- Funciones auxiliares (no se si irían en este archivo)
-- auxiliar para InsertBetween (<=)
agregarAux :: UTCTime -> UTCTime -> Descripcion -> NombreArchivo -> IO ()
agregarAux date1 date2 desc filename = if date1 <= date2
                                       then
                                         do evalComm (Insert date1 desc) filename
                                            agregarAux (addOneDay date1) date2 desc filename
                                       else return ()

-- auxiliar para InsertAllDays (<)
agregarAux2 :: UTCTime -> UTCTime -> Descripcion -> NombreArchivo -> IO ()
agregarAux2 date1 date2 desc filename = if date1 < date2
                                       then
                                         do evalComm (Insert date1 desc) filename
                                            agregarAux2 (addOneDay date1) date2 desc filename
                                       else return ()

-- auxiliar para InsertWeekly
agregarAux3 :: UTCTime -> UTCTime -> Descripcion -> NombreArchivo -> IO ()
agregarAux3 firstDay date2 desc filename = if firstDay < date2
                                           then do evalComm (Insert firstDay desc) filename
                                                   agregarAux3 (addOneWeek firstDay) date2 desc filename
                                           else return ()

printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date -- agrego 86400 segundos, o sea un día

addOneWeek :: UTCTime -> UTCTime
addOneWeek date = addUTCTime (realToFrac 604800) date -- agrego 604800, o sea una semana

formatEvent :: String -> String -> String -> String
formatEvent date time desc = date ++ "," ++ time ++ "," ++ desc ++ "\n"

getWeekDay :: String -> String
getWeekDay weekday = case weekday of "lunes"     -> "Monday"
                                     "martes"    -> "Tuesday"
                                     "miercoles" -> "Wednesday"
                                     "jueves"    -> "Thursday"
                                     "viernes"   -> "Friday"
                                     "sabado"    -> "Saturday"
                                     "domingo"   -> "Sunday"

-- devuelve la fecha del primer "weekday" del mes
-- por ejemplo: getDateByWeekDay 2020-10-01 "Wednesday" --> 2020/10/07
getDateByWeekDay :: UTCTime -> String -> UTCTime
getDateByWeekDay date weekday = if ((formatTime defaultTimeLocale "%A" date) == weekday)
                                then date
                                else getDateByWeekDay (addOneDay date) weekday
