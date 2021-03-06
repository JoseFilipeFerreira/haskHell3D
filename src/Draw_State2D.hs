{-|
Module      : Draw_State2D
Description : Module draw state in 2D haskHell 3D
-}
module Draw_State2D where

import Data_structures
import Constantes
import Map_Filter
import Enemy_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

drawAll2D:: Estado -> Picture
drawAll2D e = Rotate (-90) $ Scale 20 20 $ Pictures[ drawMap2DAll (mapa e)
                                                   , drawMap2D    (mapa e)
                                                   , drawEnemies2DAll (enemies e)
                                                   , drawEnemies2D (mapa e) (enemies e)
                                                   , drawnPlayer2D e
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

-- | draws the player in 2D
drawnPlayer2D :: Estado -> Picture
drawnPlayer2D e = Rotate (90) $ Scale 0.5 0.5 $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

-- | Changes the color of a given Wall to a given Color
paintWall:: Color -> Wall -> Wall
paintWall col w = w {wColor = col}

