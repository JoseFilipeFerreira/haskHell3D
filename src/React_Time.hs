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
import Data.Maybe
import Data.List

reactTime :: Float -> Estado -> Estado
reactTime tick e | (menu e) == MenuPlay = shootEnemy tick $ enemyDps tick $ moveWorld tick e
                 | otherwise            = e

shootEnemy :: Float -> Estado -> Estado
shootEnemy tick e | shoot (actions e) && ammo (player e) > 0 = e{ actions = (actions e){shoot = False}
                                                                , enemies = newEnemies
                                                                , player = (player e){ammo = ammo (player e) - 1}
                                                                }
                  | otherwise                                = e
    where
        newEnemies = damagedEnemy
                  ++ inLineInvisibleEnemies
                  ++ outLineEnemies
        (inLineEnemies, outLineEnemies) = partition (enemyIntercept (range(player e),0)) $ enemies e
        (inLineVisibleEnemies, inLineInvisibleEnemies) = partition (isEnemyVisible (mapa e)) inLineEnemies
        damagedEnemy = damageEnemy e $ sortOn distEnemy inLineVisibleEnemies
        
        damageEnemy :: Estado -> [Enemy] -> [Enemy]
        damageEnemy _ [] = []
        damageEnemy e (en:t) | newHP <= 0 = t
                             | otherwise  = en{hpE = newHP}:t
            where
                newHP = (hpE en) - damage(player e)


moveWorld:: Float -> Estado -> Estado
moveWorld tick e | interWall || interEnemy = rotateEnemies tick $ rotateMap tick e
                 | otherwise               = rotateEnemies tick $ moveEnemies tick $ rotateMap tick $ moveMap tick e
    where
        (vx, vy) = getVecTranslate tick e
        interWall  = any isJust $ map (wallIntercept  (-vx, -vy)) (mapa e)
        interEnemy = length (filter (enemyIntercept (-vx, -vy)) (enemies e)) > 0
        
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
rotateMap tick e = e{mapa = newMap}
                    where
                      rX = tick * (xMove $ player e)
                      newMap = map (rotateWall rX) (mapa e)

-- | Rotate a given wall by the given degrees
rotateWall::Float -> Wall -> Wall
rotateWall angDegre w = w{p1W = p1n, p2W = p2n}
    where
        (x1, y1) = p1W w
        (x2, y2) = p2W w
        ang = grauToRad angDegre
        p1n = ((x1 * cos ang - y1 * sin ang), (y1 * cos ang + x1 * sin ang))
        p2n = ((x2 * cos ang - y2 * sin ang), (y2 * cos ang + x2 * sin ang))

-- | Translate all walls
moveMap::Float -> Estado -> Estado
moveMap tick e = e{mapa = map (moveWall $ getVecTranslate tick e) (mapa e)}

-- | Translate a Wall by a given Vector
moveWall:: Vector -> Wall -> Wall
moveWall (x, y) w = w{p1W = p1n, p2W = p2n}
    where
        (x1, y1) = p1W w
        (x2, y2) = p2W w
        p1n = (x1 + x, y1 + y)
        p2n = (x2 + x, y2 + y)

-- | Rotate all the enemies in the state
rotateEnemies:: Float -> Estado -> Estado
rotateEnemies tick e = e{enemies = map (rotateEnemy rX) (enemies e)}
                    where
                      rX = tick * (xMove $ player e)

-- | Rotate a given enemy by the given degrees
rotateEnemy::Float -> Enemy -> Enemy
rotateEnemy angDegre e = e{p1E = p1n, p2E = p2n}
    where
        (x1, y1) = p1E e
        (x2, y2) = p2E e
        ang = grauToRad angDegre 
        p1n = ((x1 * cos ang - y1 * sin ang), (y1 * cos ang + x1 * sin ang))
        p2n = ((x2 * cos ang - y2 * sin ang), (y2 * cos ang + x2 * sin ang))

-- | Translate all enemies
moveEnemies:: Float -> Estado -> Estado
moveEnemies tick e = e{enemies = map (moveEnemy $ getVecTranslate tick e) (enemies e)}

-- | Translate a Enemy by a given Vector
moveEnemy:: Vector -> Enemy -> Enemy
moveEnemy (x, y) e = e{p1E = p1n, p2E = p2n}
    where
        (x1, y1) = p1E e
        (x2, y2) = p2E e
        p1n = (x1 + x, y1 + y)
        p2n = (x2 + x, y2 + y)


getVecTranslate::Float -> Estado -> (Float, Float)
getVecTranslate tick e = (vx * dist, vy * dist) 
    where
        dist = tick * walkSpeed
        (vx, vy) = unitVetorVec $ sumVec[wL, wR, w, b]
        wL   = if walkL $ actions e    then ( 0, -1) else (0, 0)
        wR   = if walkR $ actions e    then ( 0,  1) else (0, 0)
        w    = if walk $ actions e     then (-1,  0) else (0, 0)
        b    = if moonWalk $ actions e then ( 1,  0) else (0, 0)
