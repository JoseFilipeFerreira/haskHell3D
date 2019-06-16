{-|
Module      : Utils
Description : Module containing all Utils for haskHell 3D
-}
module Utils where

import Constantes
import Data_structures
import Graphics.Gloss.Geometry.Line


-- | Calculate the area that is visible to the player
viewBox :: [Coor]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (grauToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (grauToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (grauToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (grauToRad $  viewAngle/2)) 

-- | Calculate the point a given vector intersects a given Wall. Returns Nothing if it doesn't intercept.
wallIntercept :: Vector -> Wall -> Maybe Coor
wallIntercept v w = intersectSegSeg (0,0) v (p1W w) (p2W w)
 
-- | Calculate the point a given vector intersects a given Enemy. Returns Nothing if it doesn't intercept.
enemyIntercept :: Vector -> Enemy -> Maybe Coor
enemyIntercept v e = intersectSegSeg (0,0) v (p1E e) (p2E e)

unitVetorVec:: Vector -> Vector
unitVetorVec (0,0) = (0, 0) 
unitVetorVec (x,y) = ((x / bot), (y / bot)) 
    where
        bot = sqrt(x^2 + y^2)

-- | Calculate the unit vector that goes from point a to point b
unitVetor:: Coor -> Coor -> Vector
unitVetor (x1, y1) (x2, y2) = ((x / bot), (y / bot)) 
    where
        (x, y) = (x2 - x1, y2 - y1)
        bot = sqrt(x^2 + y^2)

-- | Check if a point is outside the viewBox
pointOutside::Coor -> Bool
pointOutside (x, y) = x >= farPlane
                  ||  x <= nearPlane
                  || (y <= decl1 * x + offset1)
                  || (y >= decl2 * x + offset2)
    where
        decl1 = decl (viewBox!!3) (viewBox!!0)
        offset1 = offset (viewBox!!3) decl1

        decl2 = decl (viewBox!!1) (viewBox!!2)
        offset2 = offset (viewBox!!1) decl2

        decl:: Coor -> Coor -> Float  
        decl (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 

        offset:: Coor -> Float -> Float
        offset (x, y) m = y - m * x

-- | Calculate the distance between two points
distCoor:: Coor -> Coor -> Float
distCoor (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)

-- | Sum a list of Vectors
sumVec:: [Vector] -> Vector
sumVec [] = (0,0)
sumVec ((x,y):t) = (x + tx, y + ty)
    where
        (tx, ty) = sumVec t

-- | Convert Angular vector(?) to cartesian vector (?)
vetAngToCoor:: (Float, Float) -> Vector
vetAngToCoor (a,n) = (x, y)
            where
                x = n * cos   (grauToRad a)
                y = - n * sin (grauToRad a)

-- | Convert degrees to rad
grauToRad:: Float -> Float
grauToRad x = x * pi / 180
