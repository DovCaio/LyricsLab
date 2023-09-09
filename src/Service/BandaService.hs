module Service.BandaService where

import System.IO
import Control.Exception

import Repository.BandaRepository
import Model.Banda
{-Função Pública-}
salvaBanda :: Banda -> IO()
salvaBanda banda = do
     bandasSalvas <- load
     case bandasSalvas of
        Just bandasLista -> save (banda: bandasLista)
        Nothing -> return ()

{-Função Pública-}
todasAsBandas :: IO [Banda]
todasAsBandas = do
    bandasSalvas <- load
    case bandasSalvas of
        Just bandasLista -> return bandasLista
        Nothing -> return []

{-Função Pública-}
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

{-Função Pública-}
filtrarBandaInstrumento :: String -> IO [Banda]
filtrarBandaInstrumento instrumento = do
    bandas <- todasAsBandas
    (bandasPorInstrumentos instrumento bandas []) >>= return

bandasPorInstrumentos :: String -> [Banda] -> [Banda] -> IO [Banda] 
bandasPorInstrumentos instrumento (h : t) resultado
    |null t = do
        if (temEsseInstrumento instrumento (instrumentos h)) then return (h : resultado)
        else return resultado 
    |(temEsseInstrumento instrumento (instrumentos h)) = bandasPorInstrumentos instrumento t (h: resultado)  
    |otherwise = bandasPorInstrumentos instrumento t resultado

temEsseInstrumento :: String -> [String] -> Bool
temEsseInstrumento instrumento [] = False
temEsseInstrumento instrumento (h : t) = if  instrumento == h 
    then True
    else temEsseInstrumento instrumento t

{-Função Pública-}
filtrarBandasPorGenero :: String -> IO [Banda]
filtrarBandasPorGenero genero = do
    bandas <- todasAsBandas
    (bandasPorGenero genero bandas []) >>= return

bandasPorGenero :: String -> [Banda] -> [Banda] -> IO [Banda]
bandasPorGenero generoFornecido (h : t) resultado
    |null t = do
        if (generoFornecido == (genero h)) then return (h : resultado)
        else return resultado
    |(generoFornecido == (genero h)) = bandasPorGenero generoFornecido t (h : resultado)
    |otherwise = bandasPorGenero generoFornecido t resultado

{-Função Pública-}
filtrarBandaPorArtista :: String -> IO [Banda]
filtrarBandaPorArtista artista = do
    bandas <- todasAsBandas
    (bandasPorArtista artista bandas [])

bandasPorArtista :: String -> [Banda] -> [Banda] -> IO [Banda]
bandasPorArtista artistaFornecido (h : t) resultado
    |null t = do
        if (temEsseArtista artistaFornecido (composicaoAtual h)) || (temEsseArtista artistaFornecido (artistasAnteriores h))
            then return (h : resultado)
            else return resultado
    |(temEsseArtista artistaFornecido (composicaoAtual h)) || (temEsseArtista artistaFornecido (artistasAnteriores h)) = 
        bandasPorArtista artistaFornecido t (h : resultado)
    |otherwise = bandasPorArtista artistaFornecido t resultado

temEsseArtista :: String -> [String] -> Bool
temEsseArtista artista [] = False
temEsseArtista artista (h : t) = if h == artista then True
    else temEsseArtista artista t

