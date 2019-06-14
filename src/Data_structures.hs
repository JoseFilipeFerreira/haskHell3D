{-|
Module      : Data_structures
Description : Module containig all data structures for haskHell 3D
-}
module Data_structures where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

data Estado = Estado
                { mapa     :: [Wall]
                , player   :: Player
                , actions  :: Actions
                , winSize  :: (Int, Int)
                }

data Wall = Wall
                { positionX :: Coor
                , positionY :: Coor
                , wColor     :: Color
                } deriving (Show, Eq)

type Coor = (Float, Float)

data Player = Player
                { xMove :: Float
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

