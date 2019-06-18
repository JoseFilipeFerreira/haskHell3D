{-|
Module      : Main
Description : Main Module in haskHell 3D
-}
module Main where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.Ord
import Data_structures
import React_Time
import React_Event
import Draw_State
import Constantes

main :: IO ()
main = do inicio <- initialState
          joga inicio

joga:: Estado -> IO()
joga inicio = play
        FullScreen
        (greyN 0.8)
        30
        inicio
        drawState
        reactEvent
        reactTime

initialState :: IO Estado
initialState = do  
                let defaultPlayer = Player 0 10 10 6 maximumHealth
                let defaultAction = Actions False False False False False
                return Estado { mapa    = testMap
                              , enemies = testEnemies
                              , player  = defaultPlayer
                              , actions = defaultAction
                              , menu    = MenuPlay
                              , winSize = (0,0)                                  
                              }

testEnemies::Enemies
testEnemies = [ Enemy (5 ,12) (6,12 ) 10 1 0.3
              , Enemy (11,12) (12,12) 10 1 0.3
              , Enemy (16,10) (16,9 ) 10 1 0.3
              , Enemy (14,4 ) (14,5 ) 10 1 0.3
              ]

testMap::Mapa
testMap = [ Wall (-1,-1) (-1,6)  col
          , Wall (-1,-1) (10,-1) col
          , Wall (10,-1) (10,1)  col
          , Wall (10,1) (15,1)   col
          , Wall (15,1) (15,3)   col
          , Wall (15,3) (16,3)   col
          , Wall (16,3) (16,5)   col
          , Wall (16,5) (17,5)   col
          , Wall (17,5) (17,13)  col
          , Wall (17,13) (13,13) col
          , Wall (13,13) (13,14) col
          , Wall (13,14) (1,14)  col
          , Wall (1,14) (1,8)    col
          , Wall (1,8) (4,8)     col
          , Wall (4,8) (4,6)     col
          , Wall (4,6) (-1,6)    col
          , Wall (6,6) (6,8)     col
          , Wall (6,8) (9,8)     col
          , Wall (9,8) (9,9)     col
          , Wall (9,9) (13,9)    col
          , Wall (13,9) (13,5)   col
          , Wall (13,5) (12,5)   col
          , Wall (12,5) (12,4)   col
          , Wall (12,4) (10,4)   col
          , Wall (10,4) (10,6)   col
          , Wall (10,6) (6,6)    col
          ]
    where
        col = makeColor (210/255) (180/255) (140/255) 1
