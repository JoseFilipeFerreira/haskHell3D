{-|
Module      : Map_Filter
Description : Visible walls from the player perspective
-}
module Map_Filter where

import Data_structures
import Constantes
import Utils
import Graphics.Gloss.Geometry.Line
import Data.List
import Data.Ord
import Data.Maybe

-- | The area that is visible to the player
viewBox :: [Coor]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (grauToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (grauToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (grauToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (grauToRad $  viewAngle/2)) 

-- | Get the final map with only the visible walls sorted from the farthest to the closest
getFinalMap:: Mapa -> Mapa
getFinalMap = reverse . (sortOn distWall) .filterUselessWalls

-- | Filter all the walls that are outside the viewBox or covered by other walls
filterUselessWalls:: Mapa -> Mapa
filterUselessWalls m = filterOutsideViewBox $ filter (isWallVisible m) m

-- | Filter all walls that are outside the viewBox and crop those that are in the border
filterOutsideViewBox :: Mapa -> Mapa
filterOutsideViewBox e = map fromJust $ filter isJust (map instersectWall e)

-- | Intersect a given Wall with the viewBox, returns Nothing if it is outside.
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

-- | Check if both end points of a given wall are outside the viewBox 
wallOutside::Wall -> Bool
wallOutside (Wall p1 p2 _) = (pointOutside p1) && (pointOutside p2)

-- | Check if a point is outside the viewBox
pointOutside::Coor -> Bool
pointOutside (x, y) = x >= farPlane
                  ||  x <= nearPlane
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

-- | Checks if a wall is visible from the perspective of the player
isWallVisible:: Mapa -> Wall -> Bool
isWallVisible walls w = any (id) $ map (isPointVisible filteredWalls) wallPoints
    where
        wallPoints = init $ tail $ getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls
        isPointVisible:: [Wall] -> Coor -> Bool
        isPointVisible w p = not $ any isJust $ map (wallIntercept p) w

-- | Get N points along a given Wall
getWallPoints:: Wall -> Float -> [Coor]
getWallPoints (Wall p1 p2 _) points = map (calcVec p1 vec step) [0..(points)]
    where
        step = (distCoor p1 p2) / points
        vec  = unitVetor p1 p2

        calcVec :: Coor -> Coor -> Float -> Float -> Coor
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)

-- | Calculate the distance to a given Wall
distWall:: Wall -> Float
distWall = minimum . (map (distCoor (0,0))) . (flip getWallPoints precisionWallDist)
