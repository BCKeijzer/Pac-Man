module Step where

import Collision
import Enemies
import DTypes
import Graphics.Gloss

update :: Float -> GameState -> GameState
update seconden game = case gepauzeerd game of
    WelPaused -> undefined
    NietPaused -> undefined
