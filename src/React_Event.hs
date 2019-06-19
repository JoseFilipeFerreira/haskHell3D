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
reactEvent (EventResize size)                        e = e{winSize = size}
reactEvent (EventKey (Char 'w')               s _ _) e = e{actions = (actions e){walk     = (s == Down) }}
reactEvent (EventKey (Char 's')               s _ _) e = e{actions = (actions e){moonWalk = (s == Down) }}
reactEvent (EventKey (Char 'a')               s _ _) e = e{actions = (actions e){walkL    = (s == Down) }}
reactEvent (EventKey (Char 'd')               s _ _) e = e{actions = (actions e){walkR    = (s == Down) }}
reactEvent (EventKey (SpecialKey KeyShiftL)   s _ _) e = e{actions = (actions e){run      = (s == Down) }}
reactEvent (EventKey (SpecialKey KeyEnter)    s _ _) e = e{actions = (actions e){shoot    = (s == Down) }}
reactEvent (EventKey (MouseButton LeftButton) s _ _) e = e{actions = (actions e){shoot    = (s == Down) }}
reactEvent (EventMotion (x,_))                       e = e{player  = (player  e){xMove    = x}}
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) e = e{player  = (player  e){xMove    = -arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) e = e{player  = (player  e){xMove    = nMove}}
    where
        nMove = if (xMove(player e) == -arrowRotationSpeed) then 0 else xMove $ player e
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) e = e{player  = (player  e){xMove    = arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyRight) Up   _ _) e = e{player  = (player  e){xMove    = nMove}}
    where
        nMove = if (xMove(player e) == arrowRotationSpeed) then 0 else xMove $ player e
reactEvent _ e = e
