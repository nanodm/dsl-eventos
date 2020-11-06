module Eval where

import AST
import Functions
import System.IO
import System.Directory
import Data.Char
import Data.Time
import System.IO.Unsafe

eval :: FileComm -> IO ()
eval p = evalFileComm p

-- crea un archivo dado un filename, le agrega los headers y continua evaluando comandos
evalFileComm :: FileComm -> IO ()
evalFileComm (New filename comm)  = do writeFile filename "Fecha,Horario,Descripción\n"
                                       evalComm comm filename

-- evalúa comandos de un programa dado un filename
evalFileComm (Open filename comm) = evalComm comm filename

evalComm :: Comm -> FileName -> IO ()
evalComm Skip filename = return ()
evalComm (Seq com1 com2) filename = do evalComm com1 filename
                                       evalComm com2 filename
evalComm (InsertWeekly date1 date2 desc day) filename = do 
        let eventDay = getWeekDay day
            firstDay = getDateByWeekDay date1 eventDay
        do insertWeekly firstDay date2 desc filename

evalComm (Insert date desc) filename = do
        handle <- openFile filename ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        content <- hGetContents handle
        let linedContent     = lines content
            header           = head linedContent
            events           = tail linedContent
            eventDate        = printDate date
            eventTime        = printTime date
            hasEvent         = (searchEventByFullDate eventDate eventTime events)
            hasFullDayEvent  = (searchFullDayEvent eventDate events)
        if (hasEvent == True)
        then do putStrLn $ "Ya existe un evento el " ++ eventDate ++ " a las " ++ eventTime
                hClose handle
                hClose tempHandle
                removeFile tempName
        else if (hasFullDayEvent == True)
        then do putStrLn $ "Ya existe un evento el " ++ eventDate ++ " que dura todo el día"
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines events) ++ (formatEvent eventDate eventTime desc))
                hClose handle
                hClose tempHandle
                removeFile filename
                renameFile tempName filename

evalComm (InsertBetween date1 date2 desc) filename = if date1 > date2
                                                     then putStrLn "La primer fecha debe ser menor a la segunda."
                                                     else insertBetween date1 date2 desc filename

evalComm (InsertAllDays date1 date2 desc) filename = if date1 < date2
                                                     then insertAllDays date1 date2 desc filename
                                                     else return ()

evalComm (InsertMonthly date1 date2 desc) filename = do if date1 < date2
                                                        then insertMonthly date1 date2 desc filename
                                                        else return ()
evalComm (InsertFullDay date desc) filename = do
        handle <- openFile filename ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        content <- hGetContents handle
        let linedContent = lines content
            header       = head linedContent
            events       = tail linedContent
            eventDate    = printDate date
            hasEvent     = (searchEventByDay eventDate events)
        if (hasEvent == True)
        then do putStrLn $ "Ya existe un evento el " ++ eventDate
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do hPutStr tempHandle $ (header ++ "\n" ++ (unlines events) ++ (formatEventFullDay eventDate desc))
                hClose handle
                hClose tempHandle
                removeFile filename
                renameFile tempName filename 

evalComm (SelectDate date) filename = do
        content <- readFile filename
        let linedContent   = lines content
            events         = tail linedContent
            selectedEvent  = (getEventByDate (printDate date) events)
        putStr (unlines selectedEvent)

evalComm (SelectFullDate date) filename = do
        content <- readFile filename
        let linedContent   = lines content
            events         = tail linedContent
            selectedEvent  = (getEventByFullDate (printDate date) (printTime date) events)
        putStr (unlines selectedEvent)

evalComm (SelectBetween date1 date2) filename = if date1 > date2
                                                then putStrLn "La primer fecha debe ser menor a la segunda."
                                                else selectBetween date1 date2 filename

evalComm (UpdateDescription date desc) filename = do
        handle <- openFile filename ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        content <- hGetContents handle
        let linedContent = lines content
            header       = head linedContent
            events       = tail linedContent
            newEvents    = (updateEventDescription (printDate date) (printTime date) desc events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
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
            events       = tail linedContent
            newEvents    = (updateEventFullDate (printDate date) (printTime date) (printDate newDate) (printTime newDate) events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
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
            events       = tail linedContent
            newEvents    = (updateEventDate (printDate date) (printDate newDate) events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
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
            events       = tail linedContent
            newEvents    = (updateEventTime (printDate date) (printTime date) (printTime newDate) events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para modificar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
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
            events       = tail linedContent
            newEvents    = (cancelEventByFullDate (printDate date) (printTime date) events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para cancelar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
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
            events       = tail linedContent
            newEvents    = (cancelEventByDate (printDate date) events)
        if (events == newEvents)
        then do putStrLn $ "No hay eventos para cancelar el " ++ (printDate date)
                hClose handle
                hClose tempHandle
                removeFile tempName
        else do
                hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEvents))
                hClose handle
                hClose tempHandle
                removeFile filename
                renameFile tempName filename

-- auxiliar para InsertBetween
insertBetween :: UTCTime -> UTCTime -> Description -> FileName -> IO ()
insertBetween date1 date2 desc filename = if date1 <= date2
                                          then do evalComm (Insert date1 desc) filename
                                                  insertBetween (addOneDay date1) date2 desc filename
                                          else    return ()

-- auxiliar para SelectBetween
selectBetween :: UTCTime -> UTCTime -> FileName -> IO ()
selectBetween date1 date2 filename = if date1 <= date2
                                     then do evalComm (SelectDate date1) filename
                                             selectBetween (addOneDay date1) date2 filename
                                     else    return ()

-- auxiliar para InsertAllDays
insertAllDays :: UTCTime -> UTCTime -> Description -> FileName -> IO ()
insertAllDays date1 date2 desc filename = if date1 < date2
                                          then do evalComm (Insert date1 desc) filename
                                                  insertAllDays (addOneDay date1) date2 desc filename
                                          else    return ()

-- auxiliar para InsertWeekly. Itera por cada semana del mes e inserta el evento en el día correspondiente
insertWeekly :: UTCTime -> UTCTime -> Description -> FileName -> IO ()
insertWeekly day date desc filename = if day < date
                                      then do evalComm (Insert day desc) filename
                                              insertWeekly (addOneWeek day) date desc filename
                                      else    return ()

-- auxiliar para InsertMonthly
insertMonthly :: UTCTime -> UTCTime -> Description -> FileName -> IO ()
insertMonthly date1 date2 desc filename = if date1 <= date2
                                        then do evalComm (Insert date1 desc) filename
                                                insertMonthly (utcTimeDayFix (addOneMonth date1) date2) date2 desc filename
                                        else    return ()