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
drawParts e w  [] = map (drawWall3D  e) w
drawParts e [] en = map (drawEnemy3D e) en
drawParts e (w:tm) (en:te) | dw < den  = ((drawEnemy3D e en) : (drawParts e (w:tm) te)) 
                           | otherwise = ((drawWall3D  e w)  : (drawParts e tm (en:te)))
    where
        dw  = distWall w
        den = distEnemy en

-- | Draw a given wall in 3D
drawWall3D:: Estado -> Wall -> Picture
drawWall3D e w = drawLine3D e wallHeigth (wColor w) (p1W w) (p2W w)

drawEnemy3D:: Estado -> Enemy -> Picture
drawEnemy3D e en = drawLine3D e enemyHeigth red (p1E en) (p2E en)

drawLine3D:: Estado -> Float -> Color -> Coor -> Coor -> Picture
drawLine3D e h col p1 p2 = Pictures[ Color col                 $ Polygon  allPoints
                                 , Color (contrastColor col) $ lineLoop allPoints
                                 ]
    where
        allPoints = [(xW1,pH1), (xW1,h1), (xW2,h2), (xW2,pH2)]
        (sx, sy) = winSize e
        d1  = distCoor (0,0) p1
        d2  = distCoor (0,0) p2
        hW  = h - playerHeigth
        pH1 = (playerHeigth/d1) * nearPlane * (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2)) 
        pH2 = (playerHeigth/d2) * nearPlane *  (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2))
        h1  = -(hW / d1)         * nearPlane * (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2))
        h2  = -(hW / d2)         * nearPlane * (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2))
        xW1 = xPostionPoint e p1
        xW2 = xPostionPoint e p2

        xPostionPoint :: Estado -> Coor -> Float
        xPostionPoint e (x, y) = (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2)) * nearPlane * (y/x)

-- | Draws the aim in the screen
target:: Color -> Float -> Picture
target col r =  Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        lines = [(0,0), (0,u*5), (u, u*5), (u, u), (u*5,u), (u*5, 0)]
        u = r/14
        pol = Translate (u/2) (u/2) $ Pictures[ color col                 $ Polygon lines
                                              , color (contrastColor col) $ lineLoop lines]
