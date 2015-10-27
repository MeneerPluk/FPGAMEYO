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
timeHandler time world@(World {rotateAction, movementAction, pLocation, pDirection}) 
                = world {pDirection = newRotate, pLocation = newPosition}
          where newRotate   | rotateAction == RotateLeft  = pDirection + 1.57 * time
                            | rotateAction == RotateRight = pDirection - 1.57 * time
                            | otherwise                   = pDirection
                newPosition | movementAction == Thrust    = pLocation + rotateV newRotate (1, 0)
                            | otherwise                   = pLocation