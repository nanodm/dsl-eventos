import Funciones
import System.Environment
import System.Directory
import System.IO

------ IMPORTANTE ------
-- Hay que ir descomentando los if uno por uno para probar las funciones correspondientes

---- main que modifica un evento y lo refleja en un archivo
-- main = do
--     handle <- openFile "archivo.csv" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     content <- hGetContents handle
--     let linedContent = lines content
--         header       = head linedContent
--         eventos      = tail linedContent
--         newEventos   = (modificarDescripcionEvento "09-09-2020" "21:30" "TEST" eventos)
--     hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEventos))
--     hClose handle
--     hClose tempHandle
--     removeFile "archivo.csv"
--     renameFile tempName "archivo.csv"

---- main que cancela un evento y lo refleja en un archivo
--main = do
--     handle <- openFile "archivo.csv" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     content <- hGetContents handle
--     let linedContent = lines content
--         header       = head linedContent
--         eventos      = tail linedContent
--         newEventos   = (cancelarEvento "16-09-2020" eventos)
--     hPutStr tempHandle $ (header ++ "\n" ++ (unlines newEventos))
--     hClose handle
--     hClose tempHandle
--     removeFile "archivo.csv"
--     renameFile tempName "archivo.csv"

--- main que funciona para verEventos
-- main = do
--     content <- readFile "archivo.csv"
--     let linedContent   = lines content
--         eventos        = tail linedContent
--         eventoSelected = (verEvento "09-09-2020" eventos)
--     putStrLn (unlines eventoSelected)