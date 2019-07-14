{-|
Module      : Map
Description : Module for operations over Maps
-}
module Map where

import Data_structures
import Utils

correctMap:: Mapa -> Mapa
correctMap = concatMap correctWall

correctWall:: Wall -> [Wall]
correctWall w | wS <= 1 = [w]
              | otherwise  = map (changeWallPos w (wS/fromIntegral (floor wS))) [0..(floor wS - 1)]
              where
                wS = distCoor (p1W w) (p2W w)

changeWallPos:: Wall -> Float -> Int -> Wall
changeWallPos w s n = w{p1W = n1 , p2W = n2}
    where
        (vx, vy) = unitVetor (p1W w) (p2W w)
        (px, py) = p1W w
        n1 = (px + vx * (fromIntegral n) * s, py + vy * (fromIntegral n) * s)
        n2 = (px + vx * (fromIntegral n + 1) * s, py + vy * (fromIntegral n + 1) * s)

