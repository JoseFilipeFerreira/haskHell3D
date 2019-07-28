{-|
Module      : Constantes
Description : Module containig all data structures for haskHell 3D
-}
module Constantes where

maximumHealth :: Float
maximumHealth = 8

-- | Speed the player rotates at when using the arrows
arrowRotationSpeed :: Float
arrowRotationSpeed = 100

-- | Speed the player walks at
walkSpeed :: Float
walkSpeed = 3

runSpeed :: Float
runSpeed = 7

playerHeigth::Float
playerHeigth = 0.5

wallHeigth :: Float
wallHeigth = 1

enemyHeigth :: Float
enemyHeigth = 1

-- | Angle of View
viewAngle :: Float
viewAngle = 90

-- | Distance to the near plane
nearPlane :: Float
nearPlane = 0.1

-- | Distance to the far plane
farPlane :: Float
farPlane = 30

-- | Number of points to check if a wall is visible
precisionWallHidden :: Float
precisionWallHidden = 10

-- | Number of points to calculate the distance to a wall
precisionWallDist :: Float
precisionWallDist = 10

-- | Number of points to check if a wall is visible
precisionEnemyHidden :: Float
precisionEnemyHidden = 20

-- | Number of points to calculate the distance to a wall
precisionEnemyDist :: Float
precisionEnemyDist = 10

