{-|
Module      : Enemy_Filter
Description : Visible enemies from the player perspective
-}
module Enemy_Filter where

import Data_structures
import Constantes
import Utils
import Graphics.Gloss.Geometry.Line
import Data.List
import Data.Ord
import Data.Maybe


-- | Get all the visible enemies from the farthest to the closest
getFinalEnemies:: Mapa -> Enemies -> Enemies
getFinalEnemies m = reverse . (sortOn distEnemy) . (filterUselessEnemies m)

-- | Filter all the enemies that are outside the viewBox or covered by walls
filterUselessEnemies:: Mapa -> Enemies -> Enemies
filterUselessEnemies m e = filterOutsideViewBoxEnemy $ filter (isEnemyVisible m) e

-- | Filter all enemies that are outside the viewBox and crop those that are in the border
filterOutsideViewBoxEnemy :: Enemies -> Enemies
filterOutsideViewBoxEnemy e = map fromJust $ filter isJust (map instersectEnemy e)

-- | Intersect a given enemy with the viewBox, returns Nothing if it is outside.
instersectEnemy:: Enemy -> Maybe Enemy
instersectEnemy enemy@(Enemy p1 p2 _) | enemyOutside enemy && insideViewBox = Nothing
                                      | insideViewBox                       = Just enemy
                                      | otherwise                           = Just (squashEnemy enemy)
    where
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4]
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)

        squashEnemy :: Enemy -> Enemy
        squashEnemy enemy@(Enemy p1 p2 hp) | (pointOutside p1) && (pointOutside p2) = (Enemy (closestOption p1) (closestOption p2) hp)
                                           | pointOutside p1 = (Enemy (closestOption p1) p2 hp)
                                           | pointOutside p2 = (Enemy p1 (closestOption p2) hp) 
                                           | otherwise       = enemy

        closestOption:: Coor -> Coor
        closestOption p1 = head $ sortOn (distCoor p1) $ map fromJust (filter isJust options)

-- | Check if both end points of a given enemy are outside the viewBox 
enemyOutside::Enemy -> Bool
enemyOutside (Enemy p1 p2 _) = (pointOutside p1) && (pointOutside p2)

-- | Checks if a enemy is visible from the perspective of the player
isEnemyVisible:: Mapa -> Enemy -> Bool
isEnemyVisible walls e = any (id) $ map (isPointVisible walls) enemyPoints
    where
        enemyPoints = init $ tail $ getEnemyPoints e precisionWallHidden
        isPointVisible:: [Wall] -> Coor -> Bool
        isPointVisible w p = not $ any isJust $ map (wallIntercept p) w

-- | Get N points along a given Enemy
getEnemyPoints:: Enemy -> Float -> [Coor]
getEnemyPoints (Enemy p1 p2 _) points = map (calcVec p1 vec step) [0..(points)]
    where
        step = (distCoor p1 p2) / points
        vec  = unitVetor p1 p2

        calcVec :: Coor -> Coor -> Float -> Float -> Coor
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)

-- | Calculate the distance to a given Enemy
distEnemy:: Enemy -> Float
distEnemy = minimum . (map (distCoor (0,0))) . (flip getEnemyPoints precisionEnemyDist)
