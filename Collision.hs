module Collision where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import DTypes

voedselCollision :: GameState -> GameState
voedselCollision game = game

spookCollision :: GameState -> GameState
spookCollision game = case aardbeiModus game of
    AardbeiAan -> game
    AardbeiUit -> game

aardbeiCollision :: GameState -> GameState
aardbeiCollision game = game

muurCollision :: GameState -> Bool
muurCollision game = False