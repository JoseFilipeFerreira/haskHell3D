{-|
Module      : Data_structures
Description : Module containig all data structures for haskHell 3D
-}
module Data_structures where

import Graphics.Gloss.Data.Color

-- | Game state
data Estado = Estado
                { mapa     :: Mapa
                , enemies  :: Enemies
                , player   :: Player
                , actions  :: Actions
                , menu     :: MenuPos
                , winSize  :: (Int, Int)
                }

-- | All menu positions
data MenuPos = MenuPlay
             | MenuGameOver
             deriving Eq

-- | Coordinates of a point
type Coor = (Float, Float)

-- | Define wall as the end points and it's color
data Wall = Wall
                { p1W    :: Coor
                , p2W    :: Coor
                , wColor :: Color
                } deriving (Show, Eq)

-- | Define a map as a list of Walls
type Mapa = [Wall]

-- | Define  enemy as the end points and it's HP
data Enemy = Enemy
                { p1E    :: Coor
                , p2E    :: Coor
                , hpE    :: Float
                , dpsE   :: Float
                , rangeE :: Float
                } deriving (Show, Eq)

-- | Define Enemies as a list os Enemy
type Enemies = [Enemy] 

-- | Attributes of a Player
data Player = Player
                { xMove   :: Float
                , range   :: Float
                , ammo    :: Int
                , damage  :: Float
                , hpP     :: Float
                }

-- | Actions a Player can perform
data Actions = Actions
                { walk     :: Bool
                , moonWalk :: Bool
                , walkL    :: Bool
                , walkR    :: Bool
                , shoot    :: Bool
                , run      :: Bool
                }
