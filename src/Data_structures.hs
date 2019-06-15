{-|
Module      : Data_structures
Description : Module containig all data structures for haskHell 3D
-}
module Data_structures where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

-- | Coordinates of a point
type Coor = (Float, Float)

-- | Coordinates of a vector
type Vector = (Float, Float)

-- | Define wall as the end points and it's color
data Wall = Wall
                { p1     :: Coor
                , p2     :: Coor
                , wColor :: Color
                } deriving (Show, Eq)

-- | Define a map as a list of Walls
type Mapa = [Wall]

data Estado = Estado
                { mapa     :: Mapa
                , player   :: Player
                , actions  :: Actions
                , winSize  :: (Int, Int)
                }

-- | Attributes of a Player
data Player = Player
                { xMove :: Float
                }

-- | Actions a Player can perform
data Actions = Actions
                { walk     :: Bool
                , moonWalk :: Bool
                , walkL    :: Bool
                , walkR    :: Bool
                , shoot    :: Bool
                }

-- | Images used in the game
data Images = Images
                { caca :: Picture
                , coco :: Picture
                }

