{-|
Module      : Tarefa5_2017li1g13
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where
    import Graphics.Gloss
    import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
    import Graphics.Gloss.Interface.Pure.Game
    import Data.Ord
    import Data_structures
    import Reage_Tempo
    import Reage_Evento
    import Desenha_Estado

    {-|
    Função principal usada para animar um jogo completo.
    Compilar com o GHC.
    -}
    main :: IO ()
    main = do inicio <- estadoInicial
              joga inicio
    
    -- | Função que controla o jogo
    joga:: Estado -> IO()
    joga inicio = play
          (InWindow  "haskHell 3D" (1280,720) (0,0) )                -- Janela onde irá correr o jogo      
          (greyN 0.8)                                                   -- Cor do fundo da janela.
          60                                                            -- Frame Rate
          inicio                                                        -- Fundo inicial do jogo.
          desenhaEstado                                                 -- Desenha o Estado do jogo.
          reageEvento                                                   -- Reage a um Evento.
          reageTempo                                                    -- Reage ao passar do Tempo.

    testMap::[Wall]
    testMap = [ Wall (0,0)   (10,0)  orange
              , Wall (10,0)  (10,10) red 
              , Wall (10,10) (0,10)  black
              , Wall (0, 10)  (0,0)   yellow
              ]

    -- | Função que devolve uma Picture a partir de um Estado.
    estadoInicial :: IO Estado
    estadoInicial = do  
                        let defaultPlayer = Player (7,2) 0 0
                        let defaultAction = Actions False False False False False
                        return Estado { mapa = testMap
                                      , player = defaultPlayer
                                      , actions = defaultAction
                                      , winSize = (0,0)
                                      }