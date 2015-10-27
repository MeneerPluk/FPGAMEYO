{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{pLocation, pDirection})
    = playerCharacter
  where centreX = horizontalResolution / 2
        centreY = verticalResolution   / 2
        playerCharacter = translate (pX - centreX) (pY - centreY) (rotate (radToDeg (-pDirection)) triangle)
        triangle = Color green (Polygon [(10, 0), (-10, 7), (-10, -7)])
        pX = fst pLocation
        pY = snd pLocation
