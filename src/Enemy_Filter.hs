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
instersectEnemy e | enemyOutside e && insideViewBox = Nothing
                  | insideViewBox                   = Just e
                  | otherwise                       = Just (squashEnemy e)
    where
        p1 = p1E e
        p2 = p2E e
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4]
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)

        squashEnemy :: Enemy -> Enemy
        squashEnemy e | (pointOutside p1) && (pointOutside p2) = e{p1E = closestOption p1, p2E = closestOption p2}
                      | pointOutside p1 = e{p1E = closestOption p1}
                      | pointOutside p2 = e{p2E = closestOption p2}
                      | otherwise       = e

        closestOption:: Coor -> Coor
        closestOption p1 = head $ sortOn (distCoor p1) $ map fromJust (filter isJust options)

-- | Check if both end points of a given enemy are outside the viewBox 
enemyOutside::Enemy -> Bool
enemyOutside e = (pointOutside (p1E e)) && (pointOutside (p2E e))

-- | Checks if a enemy is visible from the perspective of the player
isEnemyVisible:: Mapa -> Enemy -> Bool
isEnemyVisible walls e = any (id) $ map (isPointVisible walls) enemyPoints
    where
        enemyPoints = init $ tail $ getEnemyPoints e precisionWallHidden
        isPointVisible:: [Wall] -> Coor -> Bool
        isPointVisible w p = not $ any isJust $ map (wallIntercept p) w

