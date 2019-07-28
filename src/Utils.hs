{-|
Module      : Utils
Description : Module containing all Utils for haskHell 3D
-}
module Utils where

import Constantes
import Data_structures
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Data.Maybe

-- | Calculate the area that is visible to the player
viewBox :: [Coor]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (grauToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (grauToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (grauToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (grauToRad $  viewAngle/2)) 

-- | Calculate the point a given vector intersects a given Wall. Returns Nothing if it doesn't intercept.
wallIntercept :: Vector -> Wall -> Maybe Coor
wallIntercept v w = intersectSegSeg (0,0) v (p1W w) (p2W w)

-- | Calculate the point a given vector intersects a given Enemy. Returns Nothing if it doesn't intercept.
enemyIntercept :: Vector -> Enemy -> Bool
enemyIntercept v e = isJust $ intersectSegSeg (0,0) v (p1E e) (p2E e)

-- | Calculate the distance to a given Wall
distWall:: Wall -> Float
distWall = minimum . (map (distCoor (0,0))) . (flip getWallPoints precisionWallDist)

-- | Calculate the distance to a given Enemy
distEnemy:: Enemy -> Float
distEnemy = minimum . (map (distCoor (0,0))) . (flip getEnemyPoints precisionEnemyDist)

-- | Checks if a wall is visible from the perspective of the player
isWallVisible:: Mapa -> Wall -> Bool
isWallVisible walls w = any (id) $ map (isPointVisible filteredWalls) wallPoints
    where
        wallPoints = getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls

-- | Check if a wall is Fully visible
isWallFullyVisible:: Mapa -> [Enemy] -> Wall -> Bool
isWallFullyVisible walls en w = (all (id) $ map (isPointVisible filteredWalls) wallPoints)
                             && (all (id) $ map (isPointVisibleEnemy en) wallPoints)
                  
    where
        wallPoints = getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls

-- | Checks if a enemy is visible from the perspective of the player
isEnemyVisible:: Mapa -> Enemy -> Bool
isEnemyVisible walls e = any (id) $ map (isPointVisible walls) $ getEnemyPoints e precisionWallHidden

-- | Check if enemy is completely visible
isEnemyFullyVisible:: Mapa -> Enemy -> Bool
isEnemyFullyVisible walls e = all (id) $ map (isPointVisible walls) $ getEnemyPoints e precisionWallHidden

-- | Checks if a point is visible
isPointVisible:: [Wall] -> Coor -> Bool
isPointVisible w p = not $ any isJust $ map (wallIntercept p) w

-- | Checks if a given point is covered by any enemy
isPointVisibleEnemy:: [Enemy] -> Coor -> Bool
isPointVisibleEnemy e p = not $ any (id) $ map (enemyIntercept p) e

-- | Get N points along a given Wall
getWallPoints:: Wall -> Float -> [Coor]
getWallPoints w nP = getLinePoints (p1W w) (p2W w) nP

-- | Get N points along a given Enemy
getEnemyPoints:: Enemy -> Float -> [Coor]
getEnemyPoints e nP = getLinePoints (p1E e) (p2E e) nP 

-- | Get N points between two coordinates
getLinePoints:: Coor -> Coor -> Float -> [Coor]
getLinePoints p1 p2 nP = map (calcVec p1 vec step) [0.. nPd]
    where
        nPd = nP * (distCoor p1 p2)
        step = (distCoor p1 p2) / nPd
        vec  = unitVetor p1 p2

        calcVec :: Coor -> Coor -> Float -> Float -> Coor
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)

unitVetor:: Coor -> Coor -> Vector
unitVetor (x1, y1) (x2, y2) = normalizeV (x2 - x1, y2 - y1)

-- | Check if a point is outside the viewBox
pointOutside::Coor -> Bool
pointOutside (x, y) = x >= farPlane
                  ||  x <= nearPlane
                  || (y <= decl1 * x + offset1)
                  || (y >= decl2 * x + offset2)
    where
        decl1 = decl (viewBox!!3) (viewBox!!0)
        decl2 = decl (viewBox!!1) (viewBox!!2)
        offset1 = offset (viewBox!!3) decl1
        offset2 = offset (viewBox!!1) decl2

        decl:: Coor -> Coor -> Float  
        decl (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 

        offset:: Coor -> Float -> Float
        offset (x, y) m = y - m * x

-- | Calculate the distance between two points
distCoor:: Coor -> Coor -> Float
distCoor (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)

-- | Sum a list of Vectors
sumVec:: [Vector] -> Vector
sumVec [] = (0,0)
sumVec ((x,y):t) = (x + tx, y + ty)
    where
        (tx, ty) = sumVec t

-- | Rotates a given point by the given degrees
rotatePoint:: Float -> Coor -> Coor
rotatePoint angDegre (x,y) = ((x * cos ang - y * sin ang), (y * cos ang + x * sin ang))
    where
        ang = grauToRad angDegre

-- | Move a given point by a given vector
movePoint:: Vector -> Coor -> Coor
movePoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | Convert Angular vector(?) to cartesian vector (?)
vetAngToCoor:: (Float, Float) -> Vector
vetAngToCoor (a,n) = (x, y)
            where
                x = n * cos   (grauToRad a)
                y = - n * sin (grauToRad a)

-- | Convert degrees to rad
grauToRad:: Float -> Float
grauToRad x = x * pi / 180

-- | get a contrasting colour
contrastColor:: Color -> Color
contrastColor c | c == black = white
                | otherwise  = black
