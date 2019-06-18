{-|
Module      : React_Time
Description : Module react time haskHell 3D
-}
module React_Time where

import Data_structures
import Constantes
import Utils
import Enemy_Filter
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Data.Maybe
import Data.List

reactTime :: Float -> Estado -> Estado
reactTime tick e | (menu e) == MenuPlay = shootEnemy tick $ enemyDps tick $ moveWorld tick $ shootEnemy tick e
                 | otherwise            = e

-- | Shoot a bullet
shootEnemy :: Float -> Estado -> Estado
shootEnemy tick e | shoot (actions e) && ammo (player e) > 0 = e{ actions = (actions e){shoot = False}
                                                                , enemies = newEnemies
                                                                , player = (player e){ammo = ammo (player e) - 1}
                                                                }
                  | otherwise                                = e
    where
        newEnemies = upEnemies ++ lineInvEn ++ outEn
        (lineEn, outEn) = partition (enemyIntercept (range(player e),0)) $ enemies e
        (lineVEn, lineInvEn) = partition (isEnemyVisible (mapa e)) lineEn
        upEnemies = damageEnemy e $ sortOn distEnemy lineVEn
        
        damageEnemy :: Estado -> [Enemy] -> [Enemy]
        damageEnemy _ [] = []
        damageEnemy e (en:t) | newHP <= 0 = t
                             | otherwise  = en{hpE = newHP}:t
            where
                newHP = (hpE en) - damage(player e)

-- | move the entire world
moveWorld:: Float -> Estado -> Estado
moveWorld tick e | interWall || interEnemy = rotateEnemies tick $ rotateMap tick e
                 | otherwise               = rotateEnemies tick $ moveEnemies tick $ rotateMap tick $ moveMap tick e
    where
        (vx, vy) = getVecTranslate tick e
        interWall  = any isJust $ map (wallIntercept  (-vx, -vy)) (mapa e)
        interEnemy = length (filter (enemyIntercept (-vx, -vy)) (enemies e)) > 0

-- | Calculate all the damage from the enemies
enemyDps :: Float -> Estado -> Estado
enemyDps tick e | newHP <= 0 = e{player = (player e){hpP = 0}, menu = MenuGameOver}
                | otherwise  = e{player = (player e){hpP = newHP}}
    where
        newHP = hpP (player e) - closeEnemiesDPS * tick
        closeEnemiesDPS = sum $ map dpsE $ filter inRangeEnemy  (enemies e)

        inRangeEnemy:: Enemy -> Bool
        inRangeEnemy e = (distEnemy e) <= (rangeE e)

-- | Rotate all the walls in the map
rotateMap:: Float -> Estado -> Estado
rotateMap tick e = e{mapa = map (rotateWall (tick * (xMove $ player e))) (mapa e)}

-- | Rotate a given wall by the given degrees
rotateWall::Float -> Wall -> Wall
rotateWall a w = w{p1W = rotatePoint a (p1W w), p2W = rotatePoint a (p2W w)}

-- | Translate all walls
moveMap::Float -> Estado -> Estado
moveMap tick e = e{mapa = map (moveWall $ getVecTranslate tick e) (mapa e)}

-- | Translate a Wall by a given Vector
moveWall:: Vector -> Wall -> Wall
moveWall v w = w{p1W = movePoint v (p1W w), p2W = movePoint v (p2W w)}

-- | Rotate all the enemies in the state
rotateEnemies:: Float -> Estado -> Estado
rotateEnemies tick e = e{enemies = map (rotateEnemy rX) (enemies e)}
                    where
                      rX = tick * (xMove $ player e)

-- | Rotate a given enemy by the given degrees
rotateEnemy::Float -> Enemy -> Enemy
rotateEnemy a e = e{p1E = rotatePoint a (p1E e), p2E = rotatePoint a (p2E e)}

-- | Translate all enemies
moveEnemies:: Float -> Estado -> Estado
moveEnemies tick e = e{enemies = map (moveEnemy $ getVecTranslate tick e) (enemies e)}

-- | Translate a Enemy by a given Vector
moveEnemy:: Vector -> Enemy -> Enemy
moveEnemy v e = e{p1E = movePoint v (p1E e), p2E = movePoint v (p2E e)}

getVecTranslate::Float -> Estado -> (Float, Float)
getVecTranslate tick e = (vx * dist, vy * dist) 
    where
        dist = tick * walkSpeed
        vt = sumVec[wL, wR, w, b]
        (vx, vy) = if (vt == (0,0)) then vt else normalizeV vt
        wL   = if walkL $ actions e    then ( 0, -1) else (0, 0)
        wR   = if walkR $ actions e    then ( 0,  1) else (0, 0)
        w    = if walk $ actions e     then (-1,  0) else (0, 0)
        b    = if moonWalk $ actions e then ( 1,  0) else (0, 0)
