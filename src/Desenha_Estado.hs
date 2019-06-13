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
import           Data.Geometry.Polygon
import           Data.Geometry.Point
import qualified Data.CircularSeq as C
import           Data.Ext

-- * Desenhar um 'Estado'
-- | Função responsável por desenhar o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado e =  Scale 20 20 $ Pictures[drawnWalls, drawnPlayer e, Line viewBox]
    where
        drawnWalls = Pictures $ map drawWall $ filterOutsideWalls $ mapa e
    
drawnPlayer :: Estado -> Picture
drawnPlayer e = Rotate (90) $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

drawWall :: Wall -> Picture
drawWall (Wall p1 p2 col) = color col $ Line[p1, p2]


filterOutsideWalls:: [Wall] -> [Wall]
filterOutsideWalls = filter (isInsideWall) 
    where
        isInsideWall:: Wall -> Bool
        isInsideWall (Wall (x1, y1) (x2, y2) _) = insidePolygon (point2 x1 y1) viewBoxPoly
                                               || insidePolygon (point2 x2 y2) viewBoxPoly

viewBoxPoly :: SimplePolygon () Float
viewBoxPoly = SimplePolygon . C.fromList . map ext $ [ point2 pn1x pn1y
                                                     , point2 pn2x pn2y
                                                     , point2 pf2x pf2y
                                                     , point2 pf1x pf1y
                                                     , point2 pn1x pn1y
                                                     ]
    where
        pn1x = nearPlane
        pn1y = nearPlane * tan (grauToRad $ -viewAngle/2)
        pn2x = nearPlane
        pn2y = nearPlane * tan (grauToRad $  viewAngle/2)
        pf1x = farPlane
        pf1y = farPlane  * tan (grauToRad $ -viewAngle/2)
        pf2x = farPlane 
        pf2y = farPlane  * tan (grauToRad $  viewAngle/2) 

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
