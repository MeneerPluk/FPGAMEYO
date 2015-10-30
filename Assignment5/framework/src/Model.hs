{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Angle

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
        starLevel1       :: [Point],
        starLevel2       :: [Point],
        --Explosion (float is the direction)
        explosion        :: ([(Point, Float)], Bool),
        explosionSize    :: Int
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
                        bullets = [],
                        trail = setLifeSpan fillTrail 10,
                        pLocation = (w/2, h/2),
                        pDirection = 0,
                        enemies = zip (fillStars 20 25 (w - 25)) (drop 20 (fillStars 40 25 (h - 25))),
                        score = 0,
                        scoreMultiplier = 1,
                        pickups = [],
                        starLevel1 = zip (fillStars 80 25 (w - 25)) (drop 80 (fillStars 160 25 (h - 25))),
                        starLevel2 = zip (fillStars 240 25 (w - 25)) (drop 240 (fillStars 480 25 (h - 25))),      
                        explosion = (zip (repeat (0,0))(map degToRad (randomRs (0, 360) (mkStdGen seed))), False),
                        explosionSize = 200
                        }
        where
        fillTrail = replicate 10 ((w/2, h/2), 1)
        setLifeSpan :: [(Point, Float)] -> Float -> [(Point, Float)]
        setLifeSpan [] y = []
        setLifeSpan ((p, _):xs) y = (p, 0.1 * y) : ( setLifeSpan xs ( y - 1 ) )
        -- Amount -> MinBound -> MaxBound -> [Float]
        fillStars :: Int -> Float -> Float -> [Float]
        fillStars x min max = take x (randomRs (min, max) (mkStdGen seed))