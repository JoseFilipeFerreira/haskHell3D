{-|
Module      : Utils
Description : Module containing all Utils for haskHell 3D
-}
module Utils where

import Data_structures
import Graphics.Gloss.Geometry.Line

-- | Calculate the point a given vector intersects a given Wall. Returns Nothing if it doesn't intercept.
wallIntercept :: Vector -> Wall -> Maybe Coor
wallIntercept (px, py) (Wall pi pf _) = intersectSegSeg (0,0) (px, py) pi pf

-- | Calculate the unit vector that goes from point a to point b
unitVetor:: Coor -> Coor -> Vector
unitVetor (x1, y1) (x2, y2) = ((x / bot), (y / bot)) 
    where
        (x, y) = (x2 - x1, y2 - y1)
        bot = sqrt(x^2 + y^2)

-- | Calculate the distance between two points
distCoor:: Coor -> Coor -> Float
distCoor (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)

-- | Convert Angular vector(?) to cartesian vector (?)
vetAngToCoor:: (Float, Float) -> Vector
vetAngToCoor (a,n) = (x, y)
            where
                x = n * cos   (grauToRad a)
                y = - n * sin (grauToRad a)

-- | Convert degrees to rad
grauToRad:: Float -> Float
grauToRad x = x * pi / 180
