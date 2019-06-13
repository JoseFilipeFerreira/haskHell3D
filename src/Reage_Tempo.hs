{-|
Module      : Reage_Tempo
Description : Module react time haskHell 3D
-}
module Reage_Tempo where

    import Data_structures
    import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
    import Graphics.Gloss.Data.Color

    walkSpeed :: Float
    walkSpeed = 3

    -- * Reagir a Tempo
    -- | Função que altera o 'Estado' do jogo com base no tempo que passou 
    reageTempo :: Float -> Estado -> Estado
    reageTempo tick e = moveMap tick $ rotateMap tick e
                      
    
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
    moveMap tick e | any (wallIntercept vec) (mapa e) = e
                   | otherwise                        = e{mapa = map (moveWall vec) (mapa e)}
        where
            vec = getVecTranslate tick e

        
    wallIntercept:: (Float, Float) -> Wall -> Bool
    wallIntercept (px, py) (Wall pi pf _) = doIntersect (0,0) (-px, -py) pi pf

    getVecTranslate::Float -> Estado -> (Float, Float)
    getVecTranslate tick e | walkL $ actions e    = (xSV, ySV)
                           | walkR $ actions e    = (-xSV, -ySV)
                           | walk $ actions e     = (-xV, -yV)
                           | moonWalk $ actions e = (xV, yV)
                           | otherwise            = (0, 0)
                      where
                        dist = tick * walkSpeed
                        (xV, yV) = vetAngToCoor (ang  $ player e, dist)
                        (xSV, ySV) = vetAngToCoor (90 + ang (player e), dist)

    moveWall:: (Float, Float) -> Wall -> Wall
    moveWall (x, y) (Wall (x1, y1) (x2, y2) cor) = (Wall p1n p2n cor)
        where
            p1n = (x1 + x, y1 + y)
            p2n = (x2 + x, y2 + y)

    vetAngToCoor:: (Float, Float) -> Coor
    vetAngToCoor (a,n) = (x, y)
                where
                    x = n * cos   (grauToRad a)
                    y = - n * sin (grauToRad a)
    
    grauToRad:: Float -> Float
    grauToRad x = x * pi / 180
    
    doIntersect :: Coor -> Coor -> Coor -> Coor -> Bool
    doIntersect p1 q1 p2 q2 = isJust pInt
        where 
            pInt = intersectSegSeg p1 q1 p2 q2
