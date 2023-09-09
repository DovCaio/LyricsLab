module Model.Banda where

import Data.Aeson
import GHC.Generics



data Banda = Banda {

    nome:: String,
    composicaoAtual :: [String],
    artistasAnteriores :: [String],
    musicas :: [String],
    instrumentos :: [String],
    dataFundacao :: String,
    genero :: String


}deriving (Generic, Show)

instance FromJSON Banda
instance ToJSON Banda
