{-|
Module      : Draw_State
Description : Module draw state haskHell 3D
-}
module Draw_State where

import Data_structures
import Constantes
import Map_Filter
import Enemy_Filter
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
                          , Translate 0 (-300) $ scale 0.1 0.1 $ text $ show (hpP $ player e)
                          , Translate 0 (-315) $ scale 0.1 0.1 $ text $ show (ammo $ player e)
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
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
