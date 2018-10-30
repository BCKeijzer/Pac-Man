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

-- | Geeft true als de enemy tegen een muur aan botst
muurCollisionEnemy :: Float -> Float -> GameState -> Bool
muurCollisionEnemy xEnemy yEnemy game = not (any (mColE (xEnemy, yEnemy)) $ veld game)

muurCollision :: GameState -> Bool
muurCollision game = any (mCol $ spelerLocatie game) $ veld game

mColE :: (Float, Float) -> Field -> Bool
mColE (loperX, loperY) (Wall (x1Muur, y1Muur) (x2Muur, y2Muur)) =
    loperX + 11 >= x1Muur && loperX - 11 <= x2Muur &&
    loperY - 11 <= y1Muur && loperY + 11 >= y2Muur
mColE _ _ = True

mCol :: (Float, Float) -> Field -> Bool
mCol (loperX, loperY) (Wall (x1Muur, y1Muur) (x2Muur, y2Muur)) =
    loperX + 11 >= x1Muur && loperX - 11 <= x2Muur &&
    loperY - 11 <= y1Muur && loperY + 11 >= y2Muur
mCol _ _ = True