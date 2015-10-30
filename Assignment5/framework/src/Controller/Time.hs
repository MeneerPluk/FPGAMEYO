{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {window, rotateAction, movementAction, shootAction, pLocation, pDirection, bullets, trail, enemies, score, scoreMultiplier, starLevel1, starLevel2, explosion}) 
                = world { pDirection = newRotate, pLocation = newPosition, bullets = newBullets, trail = newTrail, enemies = newEnemies, score = newScore, scoreMultiplier = newScoreMultiplier, starLevel1 = newStarLevel1, starLevel2 = newStarLevel2, explosion = newExplosion}
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
                fst3 (a, _, _) = a
                snd3 (_, a, _) = a
                trd3 (_, _, a) = a
                
                newEnemies = snd (getShot (snd3 (chasePlayer enemies)))
                newScoreMultiplier = fst3 (chasePlayer enemies)
                
                --Return value: (Multiplier, [Enemies], Score)
                chasePlayer :: [Point] -> (Int, [Point], Int)
                chasePlayer [] = (scoreMultiplier, [], score)
                chasePlayer (x:xs) | hitPlayer (nextLocation x) = (500, [], -1)
                                   | not (hitPlayer (nextLocation x)) && (snd3 (chasePlayer xs) /= [] || xs == [])
                                        = (scoreMultiplier, nextLocation x : snd3 (chasePlayer xs), score)
                                   | otherwise = (500, [], -1)
                    where
                    --hitPlayer :: Point -> Bool
                    hitPlayer (x, y) = abs (fst newPosition - x) < 10 && abs (snd newPosition - y) < 10
                    nextLocation loc = (loc + ((dirToPlayer loc) * (100, 100) * (time, time)))
                    dirToPlayer loc = normalizeV (newPosition - loc)

                newScore | trd3 (chasePlayer enemies) /= -1 = fst (getShot (snd3 (chasePlayer enemies)))
                         | otherwise = 0
                
                -- Return value: (Score, [Enemies])
                getShot :: [Point] -> (Int, [Point])
                getShot [] = (score, [])
                getShot (x:xs)  | checkBullets newBullets x = (fst (getShot xs) + scoreMultiplier, snd (getShot xs))
                                | otherwise = (fst (getShot xs), x : snd (getShot xs))
                    where
                    checkBullets :: [(Point, Float)] -> Point -> Bool
                    checkBullets [] _ = False
                    checkBullets (((x1, y1), _):xs) (x2, y2) = (abs (x1 - x2) < 10 && abs (y1 - y2) < 10) || checkBullets xs (x2, y2)
                    
                
                --Updating the stars
                newStarLevel1 = moveStars 25 starLevel1
                newStarLevel2 = moveStars 12.5 starLevel2
                
                moveStars :: Float -> [Point] -> [Point]
                moveStars a [] = []
                moveStars a (x:xs) | (fst x) + a * time > (fst window - 25) = (25, snd x) : moveStars a xs
                                   | otherwise = (fst x + a * time, snd x) : moveStars a xs
                
                {-
                -- Explosions
                newExplosion | enemies /= [] = explode False (0,0) explosion
                             | otherwise = explode True pLocation explosion
                explode :: Bool -> Point -> ([(Point, Float)], Bool) -> ([(Point, Float)], Bool)
                explode False _ (x, False) = (x, False)
                explode True loc (ps, False) = (map (setLocation loc) ps, True)
                
                explode _ _ (ps, True) = (updateParticles ps, True)
                
                updateParticles :: [(Point, Float)] -> [(Point, Float)]
                updateParticles [] = []
                updateParticles ((loc, dir):ps) | outOfBounds loc = updateParticles ps
                                                | otherwise = (loc + rotateV dir (800 * time, 0), dir): updateParticles ps
                
                
                setLocation :: Point -> (Point, Float) -> (Point, Float)
                setLocation loc (x, dir) = (loc, dir)
                -}