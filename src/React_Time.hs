{-|
Module      : React_Time
Description : Module react time haskHell 3D
-}
module React_Time where

import Data_structures
import Constantes
import Utils
import Graphics.Gloss.Data.Color
import Data.Maybe

reactTime :: Float -> Estado -> Estado
reactTime tick e = rotateMap tick $ moveMap tick e
                  
rotateMap:: Float -> Estado -> Estado
rotateMap tick e = e{mapa = newMap}
                    where
                      rX = tick * (xMove $ player e)
                      newMap = map (rotateWall rX) (mapa e)

rotateWall::Float -> Wall -> Wall
rotateWall angDegree (Wall (x1, y1) (x2, y2) cor) = (Wall p1n p2n cor)
    where
        ang = grauToRad angDegree 
        p1n = ((x1 * cos ang - y1 * sin ang), (y1 * cos ang + x1 * sin ang))
        p2n = ((x2 * cos ang - y2 * sin ang), (y2 * cos ang + x2 * sin ang))

moveMap::Float -> Estado -> Estado
moveMap tick e | length interPoints > 0 = e
               | otherwise              = e{mapa = map (moveWall (vecx, vecy)) (mapa e)}
    where
        (vecx, vecy) = getVecTranslate tick e
        interPoints = filter isJust $ map (wallIntercept (-vecx, -vecy)) (mapa e)

getVecTranslate::Float -> Estado -> (Float, Float)
getVecTranslate tick e | walkL $ actions e    = (0    , -dist)
                       | walkR $ actions e    = (0    ,  dist)
                       | walk $ actions e     = (-dist,  0   )
                       | moonWalk $ actions e = (dist ,  0   ) 
                       | otherwise            = (0    ,  0   )
                  where
                    dist = tick * walkSpeed

-- | Translate a Wall by a given Vector
moveWall:: Vector -> Wall -> Wall
moveWall (x, y) (Wall (x1, y1) (x2, y2) cor) = (Wall p1n p2n cor)
    where
        p1n = (x1 + x, y1 + y)
        p2n = (x2 + x, y2 + y)
