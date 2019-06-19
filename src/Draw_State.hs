{-|
Module      : Draw_State
Description : Module draw state haskHell 3D
-}
module Draw_State where

import Data_structures
import Constantes
import Map_Filter
import Enemy_Filter
import Sprites
import Utils
import Draw_State3D
import Draw_State2D
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List
import Data.Ord
import Data.Maybe

drawState :: Estado -> Picture
drawState e | (menu e) == MenuPlay = drawStatePlay e
            | otherwise = Translate (-200) 0 $ scale 1 1 $ text $ "Game Over"

-- | Draw the current State
drawStatePlay :: Estado -> Picture
drawStatePlay e = Pictures[ Scale 5 5 $ drawAll3D e
                          , Translate 0 (-300) $ lives (hpP (player e)) 200
                          , Translate 0 (-340) $ ammoShow (ammo (player e)) 20
                          , Translate (-300) (-300) $ drawAll2D e
                          ]

ammoShow:: Int -> Float -> Picture
ammoShow a fS | a == 0    = Blank
              | otherwise = Scale s s $ Pictures $ map (showBullet) [1..a]
    where
        s = fS / 12
        showBullet:: Int -> Picture
        showBullet p = Translate (fromIntegral(14 * (p-1))) (-10.5) bullet

lives:: Float -> Float -> Picture
lives hp tS | hp * 8 / maximumHealth > 7    =Scale s s $ Pictures[f1, f2, f3, f4]
            | hp * 8 / maximumHealth > 6    =Scale s s $ Pictures[f1, f2, f3, h4]
            | hp * 8 / maximumHealth > 5    =Scale s s $ Pictures[f1, f2, f3]
            | hp * 8 / maximumHealth > 4    =Scale s s $ Pictures[f1, f2, h3]
            | hp * 8 / maximumHealth > 3    =Scale s s $ Pictures[f1, f2]
            | hp * 8 / maximumHealth > 2    =Scale s s $ Pictures[f1, h2]
            | hp * 8 / maximumHealth > 1    =Scale s s $ f1
            | otherwise =Scale s s $ h1
    where
        s  = tS/56
        f1 = Translate 0  (-5.5) $ heart
        h1 = Translate 0  (-5.5) $ halfHeart
        f2 = Translate 14 (-5.5) $ heart
        h2 = Translate 14 (-5.5) $ halfHeart
        f3 = Translate 28 (-5.5) $ heart
        h3 = Translate 28 (-5.5) $ halfHeart
        f4 = Translate 42 (-5.5) $ heart
        h4 = Translate 42 (-5.5) $ halfHeart

