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
drawStatePlay e = Pictures[ pla
                          , Translate 0 (-300) $ lifes (hpP (player e)) 201
                          , Translate 0 (-330) $ scale 0.1 0.1 $ text $ show (ammo $ player e)
                          ] 
    where
        pla = Rotate (-90) $ Scale 20 20 $ Pictures[ drawMap2DAll (mapa e)
                                                   , drawMap2D (mapa e)
                                                   , drawEnemies2DAll (enemies e)
                                                   , drawEnemies2D (mapa e) (enemies e)
                                                   , drawnPlayer2D e
                                                   , Line viewBox
                                                   ]

-- | Draw the final Map in 2D
drawMap2D:: Mapa -> Picture
drawMap2D  = Pictures . (map drawWall2D) . getFinalMap

-- | Draw a given Map
drawMap2DAll::Mapa -> Picture
drawMap2DAll = Pictures . (map drawWall2D) . (map (paintWall orange))

-- | Draw a given Wall in 2D
drawWall2D :: Wall -> Picture
drawWall2D w = color (wColor w) $ Line[p1W w, p2W w]

-- | Draw the final enemies
drawEnemies2D :: Mapa ->  Enemies -> Picture
drawEnemies2D m = Pictures . (map (drawEnemy2D red)) . (getFinalEnemies  m)

-- | Draw all the enemies
drawEnemies2DAll :: Enemies -> Picture
drawEnemies2DAll = Pictures . (map (drawEnemy2D green))


-- | draw a given enemy in a given color
drawEnemy2D :: Color -> Enemy -> Picture
drawEnemy2D col e = color col $ Line[p1E e, p2E e]

-- | Changes the color of a given Wall to a given Color
paintWall:: Color -> Wall -> Wall
paintWall col w = w {wColor = col}

-- | draws the player in 2D
drawnPlayer2D :: Estado -> Picture
drawnPlayer2D e = Rotate (90) $ Scale 0.5 0.5 $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        u = r/14
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (1,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]

lifes:: Float -> Float -> Picture
lifes hp tS | hp * 8 / maximumHealth > 7    =Scale s s $ Pictures[f1, f2, f3, f4]
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
