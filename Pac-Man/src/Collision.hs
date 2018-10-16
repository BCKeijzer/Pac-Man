module Collision where

import Graphics.Gloss
import DTypes

voedselCollision :: GameState -> GameState
voedselCollision = undefined

spookCollision :: GameState -> GameState
spookCollision game = case aardbeiModus game of
    AardbeiAan -> undefined
    AardbeiUit -> undefined

aardbeiCollision :: GameState -> GameState
aardbeiCollision = undefined

muurCollision :: GameState -> Bool