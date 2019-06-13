{-|
Module      : Reage_Tempo
Description : Module react time haskHell 3D
-}
module Reage_Tempo where

    import Data_structures
    import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
    import Graphics.Gloss.Data.Color
    import Graphics.Gloss.Geometry.Line
    import Data.Maybe

    walkSpeed :: Float
    walkSpeed = 3

    -- * Reagir a Tempo
    -- | Função que altera o 'Estado' do jogo com base no tempo que passou 
    reageTempo :: Float -> Estado -> Estado
    reageTempo tick e = rotateMap tick $ moveMap tick e
                      
    
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
                   | otherwise              = e{mapa = map (moveWall vec) (mapa e)}
        where
            vec = getVecTranslate tick e
            interPoints = filter isJust $ map (wallIntercept vec) (mapa e)

        
    wallIntercept :: (Float, Float) -> Wall -> Maybe Coor
    wallIntercept (px, py) (Wall pi pf _) = intersectSegSeg (0,0) (-px, -py) pi pf

    getVecTranslate::Float -> Estado -> (Float, Float)
    getVecTranslate tick e | walkL $ actions e    = (0    , -dist)
                           | walkR $ actions e    = (0    ,  dist)
                           | walk $ actions e     = (-dist,  0   )
                           | moonWalk $ actions e = (dist ,  0   ) 
                           | otherwise            = (0    ,  0   )
                      where
                        dist = tick * walkSpeed

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
