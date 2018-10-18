module Drawing where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import DTypes

window :: Display
window = InWindow "Pac-Man" (hoogteBreedte, hoogteBreedte) (offset, offset)

background :: Color
background = white

-- | Dit zorgt voor het initiele scherm
render :: GameState -> Picture
render game = pictures ([ pacman, enemy1, enemy2, enemy3, enemy4 ] ++ (map mkWall (muurLocatie game)))
    where   pacman = let (x,y) = spelerLocatie game in Color yellow $ translate x y $ circleSolid 10
            enemy1 = circleSolid 10
            enemy2 = circleSolid 10
            enemy3 = circleSolid 10
            enemy4 = circleSolid 10
            mkWall (Muur fl1 fl2 fl3 fl4) = translate fl3 fl4 $ rectangleSolid fl1 fl2