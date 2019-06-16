{-|
Module      : Draw_State
Description : Module draw state haskHell 3D
-}
module Draw_State where

import Data_structures
import Constantes
import Map_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List
import Data.Ord
import Data.Maybe

-- | Draw the current State
drawState :: Estado -> Picture
drawState e = Rotate (-90) $ Scale 20 20 $ Pictures[ drawMap2DAll (mapa e)
                                                   , drawEnemies2D (enemies e)
                                                   , drawMap2D (mapa e)
                                                   , drawnPlayer2D e
                                                   , Line viewBox
                                                   ]

-- | Draw the final Map Map in 2D
drawMap2D:: Mapa -> Picture
drawMap2D  = Pictures . (map drawWall2D) . getFinalMap

-- | Draw a given Map
drawMap2DAll::Mapa -> Picture
drawMap2DAll = Pictures . (map drawWall2D) . (map (paintWall orange))

-- | Draw a given Wall in 2D
drawWall2D :: Wall -> Picture
drawWall2D (Wall p1 p2 col) = color col $ Line[p1, p2]

drawEnemies2D :: Enemies -> Picture
drawEnemies2D = Pictures . (map drawEnemy2D) 

drawEnemy2D :: Enemy -> Picture
drawEnemy2D (Enemy p1 p2 _) = color red $ Line[p1, p2]

-- | Changes the color of a given Wall to a given Color
paintWall:: Color -> Wall -> Wall
paintWall col (Wall p1 p2 _) = (Wall p1 p2 col)

-- | draws the player in 2D
drawnPlayer2D :: Estado -> Picture
drawnPlayer2D e = Rotate (90) $ Scale 0.5 0.5 $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        u = r/14
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
