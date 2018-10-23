module Control where

import DTypes
import Collision
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | In deze functie wordt de gamestate aangepast wanneer er een key wordt gebruikt
handleKeys :: Event -> GameState -> GameState

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game 
    = game {richting = SpelerUp}
        where   spelerUp = let (x,y) = spelerLocatie game in (x, y+1)
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {richting = SpelerStill}

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game
    = game {richting = SpelerDown}
        where   spelerDown = let (x,y) = spelerLocatie game in (x, y-1)
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game {richting = SpelerStill}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game
    = game {richting = SpelerRight}
        where   spelerRight = let (x,y) = spelerLocatie game in (x + 1, y)
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game {richting = SpelerStill}

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game
    = game {richting = SpelerLeft}
    where   spelerLinks = let (x,y) = spelerLocatie game in (x - 1, y)
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game {richting = SpelerStill}
    
handleKeys (EventKey (Char 'p') Down _ _) game = case gepauzeerd game of
    NietPaused -> game { gepauzeerd = WelPaused }
    WelPaused -> game { gepauzeerd = NietPaused }

handleKeys _ game = game