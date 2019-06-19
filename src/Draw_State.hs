{-|
Module      : Draw_State
Description : Module draw state haskHell 3D
-}
module Draw_State where

import Data_structures
import Draw_State3D
import Draw_HUD
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

drawState :: Estado -> Picture
drawState e | (menu e) == MenuPlay = drawStatePlay e
            | otherwise = Translate (-200) 0 $ scale 1 1 $ text $ "Game Over"

-- | Draw the current State
drawStatePlay :: Estado -> Picture
drawStatePlay e = Pictures[ Scale 5 5 $ drawAll3D e
                          , drawHUD e
                          ]

