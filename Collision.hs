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

muurCollisionLinks :: GameState -> Bool
muurCollisionRechts :: GameState -> Bool
muurCollisionBoven :: GameState -> Bool
muurCollisionOnder :: GameState -> Bool

muurCollision :: GameState -> Bool
muurCollision game = or $ map (mCol $ spelerLocatie game) $ muurLocatie game
    where mCol (spelerx, spelery) (Muur muurX muurY muurTX muurTY) =
                muurTX - (muurX / 2) <= spelerx + 10 &&                 -- ^ linker bound
                muurTX + (muurX / 2) >= spelerx - 10 &&                 -- ^ rechter bound
                muurTY - (muurY / 2) <= spelery + 10 &&                 -- ^ lower bound
                muurTY + (muurY / 2) >= spelery - 10                    -- ^ upper bound