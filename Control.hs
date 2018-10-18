module Control where

import DTypes
import Collision
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | In deze functie wordt de gamestate aangepast wanneer er een key wordt gebruikt
handleKeys :: Event -> GameState -> GameState

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game 
 | muurCollision game = game
 | otherwise = game {spelerLocatie = (50,50)}
   where   spelerUp = (50,50)

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game
    | muurCollision game = game
    | otherwise = game {spelerLocatie = spelerDown}
        where   spelerDown = let (x,y) = spelerLocatie game in (x, y+10)

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game
    | muurCollision game = undefined
    | otherwise = game {spelerLocatie = spelerRight}
        where   spelerRight = let (x,y) = spelerLocatie game in (x + 10, y)

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game
    | muurCollision game = undefined
    | otherwise = game {spelerLocatie = spelerLinks}
    where   spelerLinks = let (x,y) = spelerLocatie game in (x - 10, y)
    
handleKeys (EventKey (Char 'p') Down _ _) game = case gepauzeerd game of
    NietPaused -> game { gepauzeerd = WelPaused }
    WelPaused -> game { gepauzeerd = NietPaused }

handleKeys _ game = game