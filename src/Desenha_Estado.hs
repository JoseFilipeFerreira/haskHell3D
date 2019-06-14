{-|
Module      : Desenha_Estado
Description : Module draw state haskHell 3D
-}
module Desenha_Estado where

import Data_structures
import Constantes
import Reage_Tempo
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Geometry.Line
import Data.List
import Data.Ord
import Data.Maybe

desenhaEstado :: Estado -> Picture
desenhaEstado e = Rotate (-90) $ Scale 20 20 $ Pictures[drawnWallsRed, drawWalls, drawnPlayer e, Line viewBox]
    where
        drawnWallsRed = Pictures $ map drawWall $ map (paintWall red) (mapa e)
        filteredOutsideWalls = filterWalls $ mapa e

        drawWalls = Pictures $ map drawWall $ filter (isWallVisible (mapa e)) filteredOutsideWalls 

paintWall:: Color -> Wall -> Wall
paintWall col (Wall p1 p2 _) = (Wall p1 p2 col)

drawnPlayer :: Estado -> Picture
drawnPlayer e = Rotate (90) $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]

drawWall :: Wall -> Picture
drawWall (Wall p1 p2 col) = color col $ Line[p1, p2]

filterWalls :: [Wall] -> [Wall]
filterWalls e = map fromJust $ filter isJust (map instersectWall e)

instersectWall:: Wall -> Maybe Wall
instersectWall wall@(Wall p1 p2 _) | wallOutside wall && insideViewBox = Nothing
                                   | insideViewBox                     = Just wall
                                   | otherwise                         = Just (squashWall wall)
    where
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4]
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)

        squashWall :: Wall -> Wall
        squashWall wall@(Wall p1 p2 col) | (pointOutside p1) && (pointOutside p2) = (Wall (closestOption p1) (closestOption p2) col)
                                         | pointOutside p1 = (Wall (closestOption p1) p2 col)
                                         | pointOutside p2 = (Wall p1 (closestOption p2) col) 
                                         | otherwise       = wall

        closestOption:: Coor -> Coor
        closestOption p1 = head $ sortOn (distCoor p1) $ map fromJust (filter isJust options)


wallOutside::Wall -> Bool
wallOutside (Wall p1 p2 _) = (pointOutside p1) && (pointOutside p2)

pointOutside::Coor -> Bool
pointOutside (x, y) = x >= farPlane
                   || x <= nearPlane
                   || (y <= decl1 * x + offset1)
                   || (y >= decl2 * x + offset2)
    where
        decl1 = decl (viewBox!!3) (viewBox!!0)
        offset1 = offset (viewBox!!3) decl1

        decl2 = decl (viewBox!!1) (viewBox!!2)
        offset2 = offset (viewBox!!1) decl2

decl:: Coor -> Coor -> Float
decl (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 

offset:: Coor -> Float -> Float
offset (x, y) m = y - m * x

viewBox :: [(Coor)]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (grauToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (grauToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (grauToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (grauToRad $  viewAngle/2)) 

isWallVisible:: [Wall] -> Wall -> Bool
isWallVisible walls w = any (id) $ map (isPointVisible filteredWalls) wallPoints
    where
        wallPoints = getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls
        isPointVisible:: [Wall] -> Coor -> Bool
        isPointVisible w p = not $ any isJust $ map (wallIntercept p) w


getWallPoints:: Wall -> Float -> [Coor]
getWallPoints (Wall p1 p2 _) points = tail $ init $ map (calcVec p1 vec step) [0..points]
    where
        step = (distCoor p1 p2) / points
        vec  = unitVetor p1 p2

        calcVec :: Coor -> Coor -> Float -> Float -> Coor
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)

unitVetor:: Coor -> Coor -> Coor
unitVetor (x1, y1) (x2, y2) = ((x / bot), (y / bot)) 
    where
        (x, y) = (x2 - x1, y2 - y1)
        bot = sqrt(x^2 + y^2)

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        u = r/14
        pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
