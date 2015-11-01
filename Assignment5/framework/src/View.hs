{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{window, pLocation, pDirection, bullets, trail, enemies, pickups, score, scoreMultiplier, starLevel1, starLevel2, explosion, explosionSize})
    = pictures (bounds : drawStarLevel1 ++ drawStarLevel2 ++ playerTrail ++ playerBullets ++ drawPickups ++ drawEnemies ++ drawExplosion ++ [playerCharacter] ++ [scoreMultiplierText] ++ [scoreText])
  where centreX = horizontalResolution / 2
        centreY = verticalResolution   / 2     
        bounds = Color red (lineLoop
                            (map translateToCenter [(25, 25), (25, (snd window) - 25), ((fst window) - 25, (snd window) - 25), ((fst window) - 25, 25)]))
        translateToCenter (x, y) = (x - centreX, y - centreY)
        
        --The following variables all pertain information relevant to the player character
        playerCharacter = translate (pX - centreX) (pY - centreY) (rotate (radToDeg (-pDirection)) playerTriangle)
        playerTriangle = Color green (Polygon [(10, 0), (-10, 7), (-10, -7)])
        pX = fst pLocation
        pY = snd pLocation
        
        playerBullet :: Point -> Picture
        playerBullet (x, y) = translate (x - centreX) (y - centreY) (Color rose (circleSolid 3))
        playerBullets = map playerBullet (map fst bullets)
        
        trailDot :: Point -> Picture
        trailDot (x, y) = translate (x - centreX) (y - centreY) (Color yellow (circleSolid 2))
        playerTrail = map trailDot (map fst trail)
        
        --Enemies
        drawEnemy (x, y) = translate (x - centreX) (y - centreY) (Color red (Rotate 45 (rectangleSolid 10 10)))
        drawEnemies = map drawEnemy enemies
        
        --Pickups
        drawPickup (x, y) = translate (x - centreX) (y - centreY) (Color blue (circleSolid 8))
        drawPickups = map drawPickup pickups
        
        --Scoreshizzle
        scoreText = translate (20 - centreX) (centreY - 40) (Scale 0.2 0.2 (Color white (text ("Score: " ++ show score))))
        scoreMultiplierText = translate (centreX - 200) (centreY - 40) (Scale 0.2 0.2 (Color white (text ("Multiplier: X" ++ show scoreMultiplier))))
        
        --Star Levels
        star1 (x, y) = translate (x - centreX) (y - centreY) (Color (greyN 0.75)  (circleSolid 2))
        drawStarLevel1 = map star1 starLevel1
        
        star2 (x, y) = translate (x - centreX) (y - centreY) (Color (greyN 0.40) (circleSolid 1))
        drawStarLevel2 = map star2 starLevel2
        
        --Draw Explosion.... or not
        drawParticle (x, y) = translate (x - centreX) (y - centreY) (Color yellow (circleSolid 1))
        drawParticles :: Int -> ([(Point, Float)], Bool) -> [Picture]
        drawParticles _ (_, False) = []
        drawParticles num (xs, True) = map drawParticle (map fst (take num xs))
        drawExplosion = drawParticles explosionSize explosion