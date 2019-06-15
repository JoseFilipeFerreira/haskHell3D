{-|
Module      : Data_structures
Description : Module containig all data structures for haskHell 3D
-}
module Constantes where

-- | Speed the player rotates at when using the arrows
arrowRotationSpeed :: Float
arrowRotationSpeed = 180

-- | Speed the player walks at
walkSpeed :: Float
walkSpeed = 3

-- | Angle of View
viewAngle :: Float
viewAngle = 90

-- | Distance to the near plane
nearPlane :: Float
nearPlane = 0.1

-- | Distance to the farplane
farPlane :: Float
farPlane = 15

-- | Number of points to check if a wall is visible
precisionWallHidden :: Float
precisionWallHidden = 20

-- | Number of points to calculate the distance to a wall
precisionWallDist :: Float
precisionWallDist = 20
