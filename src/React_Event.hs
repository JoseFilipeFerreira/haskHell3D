{-|
Module      : React_Event
Description : Module react event in haskHell 3D
-}
module React_Event where

import Data_structures
import Constantes
import Utils
import Graphics.Gloss.Interface.Pure.Game

-- | React to an event
reactEvent :: Event -> Estado -> Estado
reactEvent (EventResize size)             e = e {winSize = size}
reactEvent (EventKey (Char 'w') Down _ _) e = e{actions = (actions e){walk  = True}}
reactEvent (EventKey (Char 'w') Up   _ _) e = e{actions = (actions e){walk  = False}}
reactEvent (EventKey (Char 's') Down _ _) e = e{actions = (actions e){moonWalk  = True}}
reactEvent (EventKey (Char 's') Up   _ _) e = e{actions = (actions e){moonWalk  = False}}
reactEvent (EventKey (Char 'a') Down _ _) e = e{actions = (actions e){walkL = True}}
reactEvent (EventKey (Char 'a') Up   _ _) e = e{actions = (actions e){walkL = False}}
reactEvent (EventKey (Char 'd') Down _ _) e = e{actions = (actions e){walkR = True}}
reactEvent (EventKey (Char 'd') Up   _ _) e = e{actions = (actions e){walkR = False}}
reactEvent (EventKey (SpecialKey KeyEnter)    Down _ _) e = e{actions = (actions e){shoot = True}}
reactEvent (EventKey (SpecialKey KeyEnter)    Up   _ _) e = e{actions = (actions e){shoot = False}}
reactEvent (EventKey (MouseButton LeftButton) Down _ _) e = e{actions = (actions e){shoot = True}}
reactEvent (EventKey (MouseButton LeftButton) Up   _ _) e = e{actions = (actions e){shoot = False}}
reactEvent (EventMotion (x,_))                          e = e{player = (player e){xMove = x}}
reactEvent (EventKey (SpecialKey KeyLeft)    Down _ _)  e = e{player = (player e){xMove = -arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyLeft)    Up _ _)    e = e{player = (player e){xMove = nMove}}
    where
        nMove = if (xMove(player e) == -arrowRotationSpeed) then 0 else xMove $ player e
reactEvent (EventKey (SpecialKey KeyRight)   Down _ _)  e = e{player = (player e){xMove = arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyRight)   Up _ _)    e = e{player = (player e){xMove = nMove}}
    where
        nMove = if (xMove(player e) == arrowRotationSpeed) then 0 else xMove $ player e
reactEvent _ e = e
