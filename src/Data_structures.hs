{-|
Module      : Data_structures
Description : Module containig all data structures for haskHell 3D
-}
module Data_structures where

import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import Graphics.Gloss.Data.Color
-- | Estado do jogo:
data Estado = Estado
                { mapa     :: [Wall]
                , player   :: Player
                , actions  :: Actions
                , winSize  :: (Int, Int) -- ^ Contém o tamanho da janela em que o jogo está a correr
                }

-- | Define walls as the two ends and it's color
data Wall = Wall
                { positionX :: Coor
                , positionY :: Coor
                , wColor     :: Color
                }

type Coor = (Float, Float)

data Player = Player
                { xMove :: Float -- ^ store mouse displacement
                }

data Actions = Actions
                { walk     :: Bool
                , moonWalk :: Bool
                , walkL    :: Bool
                , walkR    :: Bool
                , shoot    :: Bool
                }

data Images = Images
                { caca :: Picture
                , coco :: Picture
                }
    
distCoor:: Coor -> Coor -> Float
distCoor (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)

