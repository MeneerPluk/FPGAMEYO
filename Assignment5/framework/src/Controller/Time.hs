{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {window, rotateAction, movementAction, shootAction, pLocation, pDirection, bullets, trail, enemies, score, scoreMultiplier, starLevel1, starLevel2}) 
                = world { pDirection = newRotate, pLocation = newPosition, bullets = newBullets, trail = newTrail, enemies = newEnemies, score, scoreMultiplier, starLevel1 = newStarLevel1, starLevel2 = newStarLevel2}
          where -- New player rotation
                newRotate   | rotateAction == RotateLeft  = pDirection + 3.14 * time
                            | rotateAction == RotateRight = pDirection - 3.14 * time
                            | otherwise                   = pDirection
                
                -- Determining the new player position
                speed       = 200
                newPosition | movementAction == Thrust    = fitToWindow (pLocation + rotateV newRotate (speed * time, 0))
                            | otherwise                   = pLocation
                fitToWindow (x, y) = (clampX x, clampY y)
                clampX a    | a < 25 = 25
                            | a > (fst window) - 25 = (fst window) - 25
                            | otherwise = a
                clampY a    | a < 25 = 25
                            | a > (snd window) - 25 = (snd window) - 25
                            | otherwise = a
                
                -- Updating the bullets
                shootBullet | shootAction == Shoot = (pLocation, pDirection) : bullets
                            | otherwise                   = bullets
                
                updateBullets :: [(Point, Float)] -> [(Point, Float)]
                updateBullets [] = []
                updateBullets (x:xs) | outOfBounds (fst (nextLocation x)) = updateBullets xs
                                              | otherwise = nextLocation x : updateBullets xs
                    where
                    nextLocation (loc, dir) = (loc + rotateV dir (800 * time, 0), dir)
                
                newBullets = updateBullets shootBullet
                outOfBounds (x, y) | x < 25
                                     || x > (fst window) - 25
                                     || y < 25
                                     || y > (snd window) - 25
                                     = True
                                   | otherwise = False

                -- Updating the trail
                updateTrail (loc, life) = if (life > 1)
                                          then (newPosition, 0)
                                          else (loc, life + time)
                newTrail = map updateTrail trail
                
                --Updating the enemies
                newEnemies = updateEnemies enemies
                updateEnemies :: [Point] -> [Point]
                updateEnemies [] = []
                updateEnemies (x:xs) | hitPlayer (nextLocation x) = -- Reset multiplier
                                                                    updateEnemies xs
                                     | otherwise = nextLocation x : updateEnemies xs
                    where
                    hitPlayer :: Point -> Bool
                    hitPlayer (x, y) | abs (fst newPosition - x) < 10 && abs (snd newPosition - y) < 10
                                        = True
                                     | otherwise = False
                    nextLocation loc = (loc + ((dirToPlayer loc) * (100, 100) * (time, time)))
                    dirToPlayer loc = normalizeV (newPosition - loc)
                
                
                --Updating the stars
                newStarLevel1 = moveStars 25 starLevel1
                newStarLevel2 = moveStars 12.5 starLevel2
                
                moveStars :: Float -> [Point] -> [Point]
                moveStars a [] = []
                moveStars a (x:xs) | (fst x) + a * time > (fst window - 25) = (25, snd x) : moveStars a xs
                                   | otherwise = (fst x + a * time, snd x) : moveStars a xs