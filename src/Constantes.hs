{-|
Module      : Constantes
Description : Module containig all data structures for haskHell 3D
-}
module Constantes where

maximumHealth :: Float
maximumHealth = 8

-- | Speed the player rotates at when using the arrows
arrowRotationSpeed :: Float
arrowRotationSpeed = 180

-- | Speed the player walks at
walkSpeed :: Float
walkSpeed = 3

runSpeed :: Float
runSpeed = 7

playerHeigth::Float
playerHeigth = 3

wallHeigth :: Float
wallHeigth = 10

enemyHeigth :: Float
enemyHeigth = 3.5

-- | Angle of View
viewAngle :: Float
viewAngle = 90

-- | Distance to the near plane
nearPlane :: Float
nearPlane = 0.1

-- | Distance to the farplane
farPlane :: Float
farPlane = 30

-- | Number of points to check if a wall is visible
precisionWallHidden :: Float
precisionWallHidden = 50

-- | Number of points to calculate the distance to a wall
precisionWallDist :: Float
precisionWallDist = 20

-- | Number of points to check if a wall is visible
precisionEnemyHidden :: Float
precisionEnemyHidden = 20

-- | Number of points to calculate the distance to a wall
precisionEnemyDist :: Float
precisionEnemyDist = 20

