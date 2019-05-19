{-|
Module      : Desenha_Estado
Description : Module draw state haskHell 3D
-}
module Desenha_Estado where

    import Data_structures
    import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
    import Graphics.Gloss.Data.Color
    import Data.List
    import Data.Ord
    import Data.Geometry.Polygon

    alturaParede :: Float
    alturaParede = 2

    near :: Float
    near = 1

    far :: Float
    far = 100

    -- * Desenhar um 'Estado'
    -- | Função responsável por desenhar o jogo.
    desenhaEstado :: Estado -> Picture
    desenhaEstado e = Pictures[
        Translate 0 (-300) $ scale 0.1 0.1 $ text $ show (coor $ player e),
        Translate 0 (-200) $ scale 0.1 0.1 $ text $ show (length cleanedMap),
        drawnWalls,
        target red 50]
        where
            cleanedMap = filter (not . invisibleWall e) (mapa e)
            sortedMap = sortOn (Down . distPlayerWall (player e)) cleanedMap
            drawnWalls = Pictures $ map (drawWall e) sortedMap
    
    drawWall :: Estado -> Wall -> Picture
    drawWall e (Wall (x1,y1) (x2,y2) col) = color col $ Polygon[(xW1,(-xW1)/5), (xW1,h1), (xW2,h2), (xW2,(-xW2)/5)]
        where
            Player (px, py) ang _ = player e
            (sx, sy) = winSize e
            d1 = distCoor (px,py) (x1,y1)
            d2 = distCoor (px,py) (x2,y2)
            h1 = (alturaParede / d1) * near * (realToFrac sy/2)
            h2 = (alturaParede / d2) * near * (realToFrac sy/2)
            xW1 = xPostionWall e (x1, y1) 
            xW2 = xPostionWall e (x2, y2) 
    
    xPostionWall :: Estado -> Coor -> Float
    xPostionWall e (x1, y1) = realToFrac sx * near * tan normWall
        where
            (sx, sy) = winSize e
            (x0, y0) = coor $ player e
            angPlayer = ang $ player e
            normWall = angPP (x0, y0) angPlayer (x1, y1)

    -- | Says if a wall is invisible from the prespective of the player
    invisibleWall :: Estado -> Wall -> Bool
    invisibleWall e (Wall (x1,y1) (x2,y2) _ ) = abs(normP1) > 45 && abs(normP2) > 45
        where
            (x0, y0) = coor $ player e
            angPlayer = ang $ player e
            normP1 = angPP (x0, y0) angPlayer (x1, y1)
            normP2 = angPP (x0, y0) angPlayer (x2, y2)
        
    angPP:: Coor -> Float -> Coor -> Float
    angPP (x0, y0) angPlayer (x1, y1) = (pi / 180) * normalize (-180) 180 (angP - angPlayer)
        where
            angP = normalize (-180) 180 ((180/pi) * atan((y1 - y0)/(x1 - x0)))

    visibleArea :: Estado -> SimplePolygon
    visibleArea e = 
        where
            (x0, y0) = coor $ player e
            angPlayer = ang $ player e

    vetAngToCoor:: VetorAng -> Vetor
    vetAngToCoor (a,n) = (x, y)
            where
                x = n * cos (ga)
                y = - n * sin (grauToRad a)

    vetCoorToAng:: Vetor -> VetorAng
    vetCoorToAng (x,y) = (angVet (x,y), n)
            where
                n = sqrt((x^2) + (y^2))

    -- | Draws the aim in the screen
    target:: Color -> Float -> Picture
    target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
        where
            u = r/14
            pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
