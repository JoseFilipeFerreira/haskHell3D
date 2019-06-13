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

    -- * Desenhar um 'Estado'
    -- | Função responsável por desenhar o jogo.
    desenhaEstado :: Estado -> Picture
    desenhaEstado e = Rotate (-90) $ Pictures[drawnWalls, drawnPlayer e]
        where
            drawnWalls = Pictures $ map drawWall (mapa e)
    
    drawnPlayer :: Estado -> Picture
    drawnPlayer e = Translate xp yp $ Rotate (a + 90) $ color red $ Scale 10 10 $ Polygon[(1,0),(-1,0),(0,1)]
        where
            (xp, yp) = coor $ player e
            a = ang $ player e

    drawWall :: Wall -> Picture
    drawWall (Wall p1 p2 col) = Scale 10 10 $ color col $ Line[p1, p2]

    -- | Draws the aim in the screen
    target:: Color -> Float -> Picture
    target col r = color col $ Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
        where
            u = r/14
            pol = Translate (u/2) (u/2) $ Polygon [(0,0), (0,u*6), (u, u*6), (u, u), (u*6,u), (u*6, 0)]
