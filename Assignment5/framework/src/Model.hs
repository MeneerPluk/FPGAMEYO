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
        pLocation        :: Point,
        pDirection       :: Float, -- Player angle, in radians.
        bullets          :: [(Point, Float)], --(Location, Direction) Speed is normalised.
        trail            :: [(Point, Float)], --(Location, time spent alive)
        --Enemy lists
        enemies          :: [Point],
        --Pickup list
        pickups          :: [Point],
        --Score things
        score            :: Int,
        scoreMultiplier  :: Int
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)

initial :: Int -> World
initial seed = World    {
                        rndGen = mkStdGen seed,
                        rotateAction = NoRotation,
                        movementAction = NoMovement,
                        shootAction = DontShoot,
                        bullets = [],
                        trail = setLifeSpan fillTrail 10,
                        pLocation = (100, 384),
                        pDirection = 0,
                        enemies = [],
                        score = 0,
                        scoreMultiplier = 1,
                        pickups = []
                        }
        where
        fillTrail = replicate 10 ((100, 384), 1)
        setLifeSpan :: [(Point, Float)] -> Float -> [(Point, Float)]
        setLifeSpan [] y = []
        setLifeSpan ((p, _):xs) y = (p, 0.1 * y) : ( setLifeSpan xs ( y - 1 ) )