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
timeHandler time world@(World {rotateAction, movementAction, shootAction, pLocation, pDirection, bullets, trail}) 
                = world {pDirection = newRotate, pLocation = newPosition, bullets = newBullets, trail = newTrail}
          where newRotate   | rotateAction == RotateLeft  = pDirection + 3.14 * time
                            | rotateAction == RotateRight = pDirection - 3.14 * time
                            | otherwise                   = pDirection
                newPosition | movementAction == Thrust    = pLocation + rotateV newRotate (speed * time, 0)
                            | otherwise                   = pLocation
                speed                                     = 200
                shootBullet | shootAction == Shoot        = (pLocation, pDirection) : bullets
                            | otherwise                   = bullets
                updateBullet (loc, dir) = (loc + rotateV dir (800 * time, 0), dir)
                newBullets = map updateBullet shootBullet
                updateTrail (loc, life) = if (life > 1)
                                          then (newPosition, 0)
                                          else (loc, life + time)
                newTrail = map updateTrail trail