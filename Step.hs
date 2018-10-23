module Step where

import Collision
import Enemies
import DTypes
import Graphics.Gloss

update :: Float -> GameState -> GameState
update seconden game = case gepauzeerd game of
    WelPaused -> game
    NietPaused -> verplaatsSpeler game

verplaatsSpeler :: GameState -> GameState
verplaatsSpeler game = case richting game of
    SpelerUp -> game {spelerLocatie = spelerUp}
        where   spelerUp
                    | muurCollision $ game {spelerLocatie = eenUp} = spelerLocatie game
                    | otherwise = eenUp
                eenUp = let (x,y) = spelerLocatie game in (x, y+1)

    SpelerDown -> game {spelerLocatie = spelerDown}
        where   spelerDown
                    | muurCollision $ game {spelerLocatie = eenDown} = spelerLocatie game
                    | otherwise = eenDown
                eenDown = let (x,y) = spelerLocatie game in (x, y-1)

    SpelerRight -> game {spelerLocatie = spelerRechts}
        where   spelerRechts
                    | muurCollision $ game {spelerLocatie = eenRechts} = spelerLocatie game
                    | otherwise = eenRechts
                eenRechts = let (x,y) = spelerLocatie game in (x+1, y)

    SpelerLeft -> game {spelerLocatie = spelerLinks}
        where   spelerLinks
                    | muurCollision $ game {spelerLocatie = eenLinks} = spelerLocatie game
                    | otherwise = eenLinks
                eenLinks = let (x,y) = spelerLocatie game in (x-1, y)

    SpelerStill -> game
