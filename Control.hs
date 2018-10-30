module Control where

import DTypes
import Collision
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | In deze functie wordt de gamestate aangepast wanneer er een key wordt gebruikt
handleKeys :: Event -> GameState -> GameState

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game 
    -- | mUp game  = game {richting = SpelerUp}
    -- | otherwise 
    = game {mogelijkerichting = MUp}          

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game
  --  | mDown game = game {richting = SpelerDown}
   -- | otherwise 
   = game {mogelijkerichting = MDown}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game
   -- | mRight game = game {richting = SpelerRight}
   -- | otherwise 
   = game {mogelijkerichting = MRight}

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game
   -- | mLeft game = game {richting = SpelerLeft}
   -- | otherwise 
   = game {mogelijkerichting = MLeft}

handleKeys (EventKey (Char 'p') Down _ _) game = case gepauzeerd game of
    NietPaused -> game { gepauzeerd = WelPaused }
    WelPaused -> game { gepauzeerd = NietPaused }

handleKeys _ game = game

