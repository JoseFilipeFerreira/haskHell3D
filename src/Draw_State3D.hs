{-|
Module      : Draw_State
Description : Module draw state in 3D for haskHell 3D
-}
module Draw_State3D where

import Data_structures
import Constantes
import Map_Filter
import Enemy_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List

-- | Draw the entire state in 3D
drawAll3D::Estado -> Picture
drawAll3D e = Pictures $ [floor, sky] ++ drawParts e m en ++ [iron]
    where
        (sxI, syI) = winSize e
        (sx, sy) = ((fromIntegral sxI) / 2, (fromIntegral syI) / 2)
        en    = getFinalEnemies (mapa e) (enemies e)
        m     = getFinalMap (mapa e)
        floor = Color (makeColor (138/255) (69/255)  (19/255)  1) $ Polygon [(-sx, 0), (sx,0), (sx, -sy), (-sx, -sy)]
        sky   = Color (makeColor (135/255) (206/255) (250/255) 1) $ Polygon [(-sx, 0), (sx,0), (sx,  sy), (-sx, sy)]
        iron  = target red $ min (fromIntegral(sxI)/22)(fromIntegral(syI)/22)


drawParts:: Estado -> Mapa -> Enemies -> [Picture]
drawParts e [] [] = []
drawParts e w  [] = map (drawWall3D e)  w
drawParts e [] en = map (drawEnemy3D e) en
drawParts e m  en | (null ven) && (null vw) && (dw < den) = ((drawEnemy3D e  (head en)) : (drawParts e  m       (tail en)))
                  | (null ven) && (null vw)               = ((drawWall3D  e  (head m )) : (drawParts e (tail m)  en      ))
                  | otherwise                             = (drawParts    e   hw hen) ++ (map (drawWall3D e) vw)++ (map (drawEnemy3D e) ven) 
    where
        dw  = distWall  (head m)
        den = distEnemy (head en)
        (ven, hen) = partition (isEnemyFullyVisible m) en
        (vw , hw ) = partition (isWallFullyVisible  m en) m


-- | Draw a given wall in 3D
drawWall3D:: Estado -> Wall -> Picture
drawWall3D e w = drawLine3D e wallHeigth (wColor w) (p1W w) (p2W w)

-- | Draw a given enemy in 3D
drawEnemy3D:: Estado -> Enemy -> Picture
drawEnemy3D e en = drawLine3D e enemyHeigth red (p1E en) (p2E en)

-- | Draw a line in 3D
drawLine3D:: Estado -> Float -> Color -> Coor -> Coor -> Picture
drawLine3D e h col p1 p2 = Pictures[ Color col                 $ Polygon  (getCornerPoints e h p1 p2)
                                   , Color (contrastColor col) $ lineLoop (getCornerPoints e h p1 p2)
                                   ]

getCornerPoints::Estado -> Float -> Coor -> Coor -> [Coor]
getCornerPoints e h (x1, y1) (x2, y2) = [(xW1,pH1), (xW1,h1), (xW2,h2), (xW2,pH2)] 
    where
        d1  = distCoor (0,0) (x1, y1)
        d2  = distCoor (0,0) (x2, y2)
        hW  = h - playerHeigth
        ratio = nearPlane * (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2))
        pH1 =  (playerHeigth/d1) * ratio 
        pH2 =  (playerHeigth/d2) * ratio
        h1  = -(hW / d1)         * ratio
        h2  = -(hW / d2)         * ratio
        xW1 =  (y1 / x1)         * ratio
        xW2 =  (y2 / x2)         * ratio

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r =  Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        lines = [(0,0), (0,u*5), (u, u*5), (u, u), (u*5,u), (u*5, 0)]
        u = r/14
        pol = Translate (u/2) (u/2) $ Pictures[ color col                 $ Polygon lines
                                              , color (contrastColor col) $ lineLoop lines]
