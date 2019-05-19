{-|
Module      : Reage_Tempo
Description : Module react time haskHell 3D
-}
module Reage_Tempo where

    import Data_structures
    import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
    import Graphics.Gloss.Data.Color

    -- * Reagir a Tempo
    -- | Função que altera o 'Estado' do jogo com base no tempo que passou 
    reageTempo :: Float -> Estado -> Estado
    reageTempo tick e = movePlayer tick $ rotatePlayer tick e
                      
    
    rotatePlayer::Float -> Estado -> Estado
    rotatePlayer tick e = e{player = (player e){ang = normalize (-180) 180 (ang(player e) + displace)}}
                        where
                          (sX, _) = winSize e
                          rX = xMove $ player e
                          displace = 20 * rX / realToFrac sX
    
    movePlayer::Float -> Estado -> Estado
    movePlayer tick e | walkL $ actions e    = e{player = (player e){coor = (xP + xSV, yP + ySV)}}
                      | walkR $ actions e    = e{player = (player e){coor = (xP - xSV, yP - ySV)}}
                      | walk $ actions e     = e{player = (player e){coor = (xP + xV, yP + yV)}}
                      | moonWalk $ actions e = e{player = (player e){coor = (xP - xV, yP - yV)}}
                      | otherwise            = e
                      where
                        (xP, yP) = coor $ player e
                        (xV, yV) = vetAngToCoor (ang  $ player e, tick)
                        (xSV, ySV) = vetAngToCoor (90 + ang (player e), tick)

    vetAngToCoor:: (Float, Float) -> Coor
    vetAngToCoor (a,n) = (x, y)
                where
                    x = n * cos   (grauToRad a)
                    y = - n * sin (grauToRad a)
    
    grauToRad:: Float -> Float
    grauToRad x = x * pi / 180
    