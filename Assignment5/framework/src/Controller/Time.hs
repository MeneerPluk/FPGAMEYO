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
timeHandler time world@(World {window, rotateAction, movementAction, shootAction, reloadTimer, pLocation, pDirection, bullets, trail}) 
                = world {reloadTimer = newReloadTimer, pDirection = newRotate, pLocation = newPosition, bullets = newBullets, trail = newTrail}
          where -- New player rotation
                newRotate   | rotateAction == RotateLeft  = pDirection + 3.14 * time
                            | rotateAction == RotateRight = pDirection - 3.14 * time
                            | otherwise                   = pDirection
                
                -- Determining the new player position
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
                newReloadTimer | reloadTimer > 0.05 = 0
                               | otherwise = reloadTimer + time
                               
                shootBullet | shootAction == Shoot
                              && newReloadTimer == 0       = (pLocation, pDirection) : bullets
                            | otherwise                   = bullets
                
                updateBullets :: [(Point, Float)] -> [(Point, Float)]
                updateBullets [] = []
                updateBullets ((loc, dir):xs) | outOfBounds loc = xs
                                              | otherwise = (loc + rotateV dir (800 * time, 0), dir) : updateBullets xs
                
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