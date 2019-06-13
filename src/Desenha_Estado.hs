{-|
Module      : Desenha_Estado
Description : Module draw state haskHell 3D
-}
module Desenha_Estado where

import           Data_structures
import           Constantes
import           Reage_Tempo
import           Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import           Graphics.Gloss.Data.Color
import           Data.List
import           Data.Ord
import           Data.Maybe

-- * Desenhar um 'Estado'
-- | Função responsável por desenhar o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado e = Rotate (-90) $ Scale 20 20 $ Pictures[drawnWalls, drawnPlayer e, Line viewBox]
    where
        drawnWalls = Pictures $ map drawWall $ filterWalls $ mapa e
    
drawnPlayer :: Estado -> Picture
drawnPlayer e = Rotate (90) $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

drawWall :: Wall -> Picture
drawWall (Wall p1 p2 col) = color col $ Line[p1, p2]

filterWalls :: [Wall] -> [Wall]
filterWalls = filter (isJust . instersectWall)

instersectWall:: Wall -> Maybe Wall
instersectWall wall@(Wall (x1, y1) (x2, y2) cor) | outsidePlaneX = Nothing
                                                 | otherwise     = Just wall
    where
        outsidePlaneX = (x1 > farPlane || x1 < nearPlane)
                     && (x2 > farPlane || x2 < nearPlane)

viewBox :: [(Coor)]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (grauToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (grauToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (grauToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (grauToRad $  viewAngle/2)) 

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        u = r/14
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
