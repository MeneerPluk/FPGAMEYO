{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Point

-- | Game state

data World = World {
        --World size
        window           :: Point,
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        --Player Details
        reloadTimer      :: Float, -- To keep the amount of bullets in check
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
        scoreMultiplier  :: Int,
        --Stars
        starLevel1       :: [Float],
        starLevel2       :: [Float]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
    deriving (Eq)
data MovementAction = NoMovement | Thrust
    deriving (Eq)
data ShootAction    = Shoot      | DontShoot
    deriving (Eq)

initial :: Int -> Point -> World
initial seed (w, h) = World    {
                        window = (w, h),
                        rndGen = mkStdGen seed,
                        rotateAction = NoRotation,
                        movementAction = NoMovement,
                        shootAction = DontShoot,
                        reloadTimer = 0.4,
                        bullets = [],
                        trail = setLifeSpan fillTrail 10,
                        pLocation = (w/2, h/2),
                        pDirection = 0,
                        enemies = [],
                        score = 0,
                        scoreMultiplier = 1,
                        pickups = [],
                        starLevel1 = fillStars 80,
                        starLevel2 = fillStars 240
                        }
        where
        fillTrail = replicate 10 ((w/2, h/2), 1)
        setLifeSpan :: [(Point, Float)] -> Float -> [(Point, Float)]
        setLifeSpan [] y = []
        setLifeSpan ((p, _):xs) y = (p, 0.1 * y) : ( setLifeSpan xs ( y - 1 ) )
        
fillStars :: Int -> [Float]
fillStars x = take x (randomRs (0, w) rndGen)