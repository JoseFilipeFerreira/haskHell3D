{-|
Module      : Desenha_Estado
Description : Module draw state haskHell 3D
-}
module Desenha_Estado where

import Data_structures
import Constantes
import Map_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List
import Data.Ord
import Data.Maybe

desenhaEstado :: Estado -> Picture
desenhaEstado e = Rotate (-90) $ Scale 20 20 $ Pictures[drawMap2DRed (mapa e), drawMap2D (mapa e), drawnPlayer e, Line viewBox]

drawMap2D:: Mapa -> Picture
drawMap2D  = Pictures . (map drawWall2D) . getFinalMap

drawMap2DRed::Mapa -> Picture
drawMap2DRed = Pictures . (map drawWall2D) . (map (paintWall red))

paintWall:: Color -> Wall -> Wall
paintWall col (Wall p1 p2 _) = (Wall p1 p2 col)

drawnPlayer :: Estado -> Picture
drawnPlayer e = Rotate (90) $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

drawWall2D :: Wall -> Picture
drawWall2D (Wall p1 p2 col) = color col $ Line[p1, p2]


-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        u = r/14
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
