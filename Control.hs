module Control where

import DTypes
import Collision
import Graphics.Gloss

-- | In deze functie wordt de gamestate aangepast wanneer er een key wordt gebruikt
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game 
    | muurCollision game {spelerLocatie = undefined} = undefined
    | otherwise = undefined

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game
    | muurCollision game {spelerLocatie = undefined} = undefined
    | otherwise = undefined

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game
    | muurCollision game {spelerLocatie = undefined} = undefined
    | otherwise = undefined

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game
    | muurCollision game {spelerLocatie = undefined} = undefined
    | otherwise = undefined
    
handleKeys (EventKey (Char 'p') Down _ _) game = case gepauzeerd game of
    NietPaused -> game { gepauzeerd = WelPaused }
    WelPaused -> game { gepauzeerd = NeitPaused }