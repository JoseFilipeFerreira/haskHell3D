{-|
Module      : Reage_Evento
Description : Module react time haskHell 3D
-}
module Reage_Evento where

import Data_structures
import Constantes
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game


reageEvento :: Event -> Estado -> Estado
reageEvento (EventResize size)             e = e {winSize = size}
reageEvento (EventKey (Char 'w') Down _ _) e = e{actions = (actions e){walk  = True}}
reageEvento (EventKey (Char 'w') Up   _ _) e = e{actions = (actions e){walk  = False}}
reageEvento (EventKey (Char 's') Down _ _) e = e{actions = (actions e){moonWalk  = True}}
reageEvento (EventKey (Char 's') Up   _ _) e = e{actions = (actions e){moonWalk  = False}}
reageEvento (EventKey (Char 'a') Down _ _) e = e{actions = (actions e){walkL = True}}
reageEvento (EventKey (Char 'a') Up   _ _) e = e{actions = (actions e){walkL = False}}
reageEvento (EventKey (Char 'd') Down _ _) e = e{actions = (actions e){walkR = True}}
reageEvento (EventKey (Char 'd') Up   _ _) e = e{actions = (actions e){walkR = False}}
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) e = e{actions = (actions e){shoot = True}}
reageEvento (EventKey (SpecialKey KeyEnter)    Up   _ _) e = e{actions = (actions e){shoot = False}}
reageEvento (EventKey (MouseButton LeftButton) Down _ _) e = e{actions = (actions e){shoot = True}}
reageEvento (EventKey (MouseButton LeftButton) Up   _ _) e = e{actions = (actions e){shoot = False}}
reageEvento (EventMotion (x,_))                          e = e{player = (player e){xMove = x}}
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _)  e = e{player = (player e){xMove = -arrowRotationSpeed}}
reageEvento (EventKey (SpecialKey KeyLeft)    Up _ _)    e = e{player = (player e){xMove = nMove}}
    where
        nMove = if (xMove(player e) == -arrowRotationSpeed) then 0 else xMove $ player e
reageEvento (EventKey (SpecialKey KeyRight)   Down _ _)  e = e{player = (player e){xMove = arrowRotationSpeed}}
reageEvento (EventKey (SpecialKey KeyRight)   Up _ _)    e = e{player = (player e){xMove = nMove}}
    where
        nMove = if (xMove(player e) == arrowRotationSpeed) then 0 else xMove $ player e
reageEvento _ e = e
