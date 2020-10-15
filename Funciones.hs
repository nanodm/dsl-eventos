module Funciones
( agregar,
  verEvento,
  verEvento2,
  cancelarEvento,
  cancelarEvento2,
  modificarDescripcionEvento
) where

import Data.Dates
import Data.Time

--- función auxiliar que agarra una linea separada por comas y devuelve una lista con las palabras de la misma
--- por ej: > sepByComma "09-09-2020,21:30,Cocinar"
--- ["09-09-2020","21:30","Cocinar"]
-- Sintaxis robada de la función words de Data.List -> https://www.haskell.org/onlinereport/standard-prelude.html
sepByComma     :: String -> [String]
sepByComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : sepByComma s''
                            where (w, s'') = break (==',') s'
-- dropWhile: crea una lista a partir de otra, inspecciona la lista original y toma de ella sus elementos desde el momento en que la condición falla por primera vez hasta el final de la lista

getFechaHoraDescripcion :: String -> [String]
getFechaHoraDescripcion evento = sepByComma evento

------ INSERT
agregar :: String -> String -> String -> IO ()
agregar fecha hora descripcion = appendFile "archivo.csv" (fecha ++ "," ++ hora ++ "," ++ descripcion ++ "\n")

------ SELECT
-- ver "dia-mes-año"
verEvento :: String -> [String] -> [String]
verEvento date []     = []
verEvento date (x:xs) =
       let fechaHoraDescripcion = getFechaHoraDescripcion x
           fecha                = fechaHoraDescripcion !! 0 -- la función !! devuelve un elemento de una lista dado un índice
           evento               = if fecha == date then (x : verEvento date xs) else verEvento date xs
       in evento

-- ver "dia-mes-año" "hora:min"
verEvento2 :: String -> String -> [String] -> [String]
verEvento2 date time []     = []
verEvento2 date time (x:xs) =
       let fechaHoraDescripcion = getFechaHoraDescripcion x
           fecha                = fechaHoraDescripcion !! 0
           hora                 = fechaHoraDescripcion !! 1
           evento               = if (fecha == date && hora == time) then x : verEvento2 date time xs else verEvento2 date time xs
       in evento

------ DELETE
-- cancelar "dia-mes-año"
cancelarEvento :: String -> [String] -> [String]
cancelarEvento _ []        = []
cancelarEvento date (x:xs) =
       let fechaHoraDescripcion = getFechaHoraDescripcion x
           fecha                = fechaHoraDescripcion !! 0
           evento               = if fecha == date then (cancelarEvento date xs) else (x : cancelarEvento date xs)
       in evento

-- cancelar "dia-mes-año" "hora:min"
cancelarEvento2 :: String -> String -> [String] -> [String]
cancelarEvento2 date time []     = []
cancelarEvento2 date time (x:xs) =
       let fechaHoraDescripcion = getFechaHoraDescripcion x
           fecha                = fechaHoraDescripcion !! 0
           hora                 = fechaHoraDescripcion !! 1
           evento               = if (fecha == date && hora == time)
                                  then (cancelarEvento2 date time xs)
                                  else (x : cancelarEvento2 date time xs)
       in evento

------ UPDATE
-- modificar "dia-mes-año" "hora:min" "descripcion nueva"
modificarDescripcionEvento :: String -> String -> String -> [String] -> [String]
modificarDescripcionEvento _ _ _ [] = []
modificarDescripcionEvento date time newVal (x:xs) =
       let fechaHoraDescripcion = getFechaHoraDescripcion x
           fecha                = fechaHoraDescripcion !! 0
           hora                 = fechaHoraDescripcion !! 1
           nuevaLista           = if (fecha == date && hora == time)
                                  then (date ++ "," ++ hora ++ "," ++ newVal):xs
                                  else x:(modificarDescripcionEvento date time newVal xs)
       in nuevaLista

-- TODO: cancelar eventos entre 2 fechas