module Drawing where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import DTypes

window :: Display
window = InWindow "Pac-Man" (hoogteBreedte, hoogteBreedte) (offset, offset)

background :: Color
background = light $ light $ light blue

-- | Dit zorgt voor het initiele scherm
render :: GameState -> IO Picture
render game = return $ pictures $ [ pacman, enemy1, enemy2, enemy3, enemy4 ] ++ concat (map mkWall (veld game))
    where   pacman = let (x,y) = spelerLocatie game in Color yellow $ translate x y $ circleSolid 10
            enemy1 = let (x, y) = enemyLocatie1 game in Color blue $ translate x y $ circleSolid 10
            enemy2 = let (x, y) = enemyLocatie2 game in Color blue $ translate x y $ circleSolid 10
            enemy3 = let (x, y) = enemyLocatie3 game in Color blue $ translate x y $ circleSolid 10
            enemy4 = let (x, y) = enemyLocatie4 game in Color blue $ translate x y $ circleSolid 10
            mkWall (Wall (fl1, fl2) (_, _)) = [translate (fl1 + 12) (fl2 - 12) $ rectangleSolid 24 24]
            mkWall _ = []