module Service.BandaService where

import System.IO
import Control.Exception

import Repository.BandaRepository
import Model.Banda

salvaBanda :: Banda -> IO()
salvaBanda banda = do
     bandasSalvas <- load
     case bandasSalvas of
        Just bandasLista -> save (banda: bandasLista)
        Nothing -> return ()

todasAsBandas :: IO [Banda]
todasAsBandas = do
    bandasSalvas <- load
    case bandasSalvas of
        Just bandasLista -> return bandasLista
        Nothing -> return []


recuperarPorNome :: String -> IO Banda
recuperarPorNome nome = do
    bandas <- todasAsBandas
    if null bandas then return (Banda "null" ["null","null"] ["null"] ["null"]  ["null"] "null" "null")
    else (recuperandoPorNome nome bandas) >>= return


recuperandoPorNome :: String -> [Banda] -> IO Banda
recuperandoPorNome nomeBanda (h:t)
    |null t = do
        if nomeBanda == (nome h) then return h
        else return  (Banda "null" ["null","null"] ["null"] ["null"]  ["null"] "null" "null")
    | nomeBanda == (nome h) = return h
    | otherwise = recuperandoPorNome nomeBanda t >>= return

filtrarBandaInstrumento :: String -> IO [Banda]
filtrarBandaInstrumento instrumento = do
    bandas <- todasAsBandas
    bandasPorInstrumentos instrumento bandas >>= return

bandasPorInstrumentos :: String -> [Bandas] -> IO [Banda] 
bandasPorInstrumentos instrumento bandas
    |