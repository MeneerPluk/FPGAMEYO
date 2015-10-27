{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Point

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        --Player Details
        pLocation :: Point,
        pDirection :: Float, -- Player angle, in radians.
        bullets :: [(Point, Float)], --(Location, Direction) Speed is normalised.
        --Enemy lists
        enemies :: [Point]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot

initial :: Int -> World
initial seed = World    {
                        rndGen = mkStdGen seed,
                        rotateAction = NoRotation,
                        movementAction = NoMovement,
                        shootAction = DontShoot,
                        bullets = [],
                        pLocation = (100, 384),
                        pDirection = 0,
                        enemies = []
                        }
