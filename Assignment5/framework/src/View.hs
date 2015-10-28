{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{pLocation, pDirection, bullets, score, scoreMultiplier})
    = pictures (scoreMultiplierText : scoreText : playerCharacter : playerBullets)
  where centreX = horizontalResolution / 2
        centreY = verticalResolution   / 2       
        --The following variables all pertain information relevant to the player character
        playerCharacter = translate (pX - centreX) (pY - centreY) (rotate (radToDeg (-pDirection)) playerTriangle)
        playerTriangle = Color green (Polygon [(10, 0), (-10, 7), (-10, -7)])
        pX = fst pLocation
        pY = snd pLocation
        playerBullet :: Point -> Picture
        playerBullet (x, y) = translate (x - centreX) (y - centreY) (Color blue (circleSolid 3))
        playerBullets = map playerBullet (map fst bullets)
        
        --Scoreshizzle
        scoreText = translate (20 - centreX) (centreY - 40) (Scale 0.2 0.2 (Color white (text ("Score: " ++ show score))))
        scoreMultiplierText = translate (centreX - 200) (centreY - 40) (Scale 0.2 0.2 (Color white (text ("Multiplier: X" ++ show scoreMultiplier))))