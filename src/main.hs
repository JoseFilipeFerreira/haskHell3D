{-|
Module      : Tarefa5_2017li1g13
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.Ord
import Data_structures
import Reage_Tempo
import Reage_Evento
import Desenha_Estado

main :: IO ()
main = do inicio <- initialState
          joga inicio

joga:: Estado -> IO()
joga inicio = play
        (InWindow  "haskHell 3D" (1280,720) (0,0))
        (greyN 0.8)
        30
        inicio
        desenhaEstado
        reageEvento
        reageTempo

initialState :: IO Estado
initialState = do  
                let defaultPlayer = Player 0
                let defaultAction = Actions False False False False False
                return Estado { mapa = testMap
                              , player = defaultPlayer
                              , actions = defaultAction
                              , winSize = (0,0)                                  
                              }

testMap::[Wall]
testMap = [ Wall (-1,-1) (-1,6)  black
          , Wall (-1,-1) (10,-1) black
          , Wall (10,-1) (10,1) black
          , Wall (10,1) (15,1) black
          , Wall (15,1) (15,3) black
          , Wall (15,3) (16,3) black
          , Wall (16,3) (16,5) black
          , Wall (16,5) (17,5) black
          , Wall (17,5) (17,13) black
          , Wall (17,13) (13,13) black
          , Wall (13,13) (13,14) black
          , Wall (13,14) (1,14) black
          , Wall (1,14) (1,8) black
          , Wall (1,8) (4,8) black
          , Wall (4,8) (4,6) black
          , Wall (4,6) (-1,6) black
          , Wall (6,6) (6,8) black
          , Wall (6,8) (9,8) black
          , Wall (9,8) (9,9) black
          , Wall (9,9) (13,9) black
          , Wall (13,9) (13,5) black
          , Wall (13,5) (12,5) black
          , Wall (12,5) (12,4) black
          , Wall (12,4) (10,4) black
          , Wall (10,4) (10,6) black
          , Wall (10,6) (6,6) black
          ]
