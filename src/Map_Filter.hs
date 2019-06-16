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
instersectWall w | wallOutside w && insideViewBox = Nothing
                 | insideViewBox                  = Just w
                 | otherwise                      = Just (squashWall w)
    where
        p1 = p1W w
        p2 = p2W w
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4]
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)

        squashWall :: Wall -> Wall
        squashWall w | wallOutside w        = w{p1W = closestOption (p1W w), p2W = closestOption (p2W w)}
                     | pointOutside (p1W w) = w{p1W = closestOption (p1W w)}
                     | pointOutside (p2W w) = w{p2W = closestOption (p2W w)}
                     | otherwise            = w

        closestOption:: Coor -> Coor
        closestOption p1 = head $ sortOn (distCoor p1) $ map fromJust (filter isJust options)

-- | Check if both end points of a given wall are outside the viewBox 
wallOutside::Wall -> Bool
wallOutside w = pointOutside (p1W w) && pointOutside (p2W w)

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
getWallPoints w points = map (calcVec p1 vec step) [0..(points)]
    where
        p1 = p1W w
        p2 = p2W w
        step = (distCoor p1 p2) / points
        vec  = unitVetor p1 p2

        calcVec :: Coor -> Coor -> Float -> Float -> Coor
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)

-- | Calculate the distance to a given Wall
distWall:: Wall -> Float
distWall = minimum . (map (distCoor (0,0))) . (flip getWallPoints precisionWallDist)
