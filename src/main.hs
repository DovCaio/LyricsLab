import Model.Banda
import Repository.BandaRepository
import Service.BandaService

main:: IO()
main = do
    
    --salvaBanda (Banda "Salvo3" ["Eu2","tu2"] ["Eu2"] ["dea2"]  ["violao2"] "12/12/12" "rock2")
    
    --bd <- todasAsBandas
    --print (bd)
    
    --banda <- recuperarPorNome "Salvo2"
    --print banda

    --bandas <- filtrarBandaInstrumento "1"
    --print bandas

    --bandas <- filtrarBandasPorGenero "rock"
    --print bandas

    --bandas <- filtrarBandaPorArtista "Jota"

    --print bandas

    --removerBanda "Salvo3"

    --atualizaBanda "Salvo2" (Banda "Salvo3333" ["Eu233","tu233"] ["Eu233"] ["dea2"]  ["violao2"] "12/12/12" "rock2")

    --existe <- existeBanda "Salvo333"

    --print existe

    --banda <- adicionarInstrumento "Guitarra" "Salvo3333"

    --print banda

    --banda <- adicionaNovoIntegrante "NOVOINTEGRANTE" "Salvo3333"

    --print banda

    --banda <- removeIntegrante "NOVOINTEGRANTE" "Salvo3333"

    --print banda