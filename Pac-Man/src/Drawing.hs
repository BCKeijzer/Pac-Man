module Drawing where

import Graphics.Gloss
import DTypes

window :: Display
window = InWindow "Pac-Man" (hoogteBreedte, hoogteBreedte) (offset, offset)

background :: Color
background = undefined

-- | Dit zorgt voor het initiele scherm
render :: GameState -> Picture
render = undefined