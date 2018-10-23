module Enemies where

import Collision
import Graphics.Gloss
import DTypes

moveEnemy1 :: GameState -> GameState
moveEnemy1 game = game

moveEnemy2 :: GameState -> GameState
moveEnemy2 game = game

moveEnemy3 :: GameState -> GameState
moveEnemy3 game = game

moveEnemy4 :: GameState -> GameState
moveEnemy4 game = game

enemyDichtsteBij :: Float -> Float -> Float -> Float -> ERichting -> GameState -> ERichting
enemyDichtsteBij xEnemy yEnemy xDoel yDoel richtingEnemy game
 | dxyRechts    <= minimum rest && muurCollisionEnemy (xEnemy + 1) yEnemy game && nOpposite ERight          = ERight
 | dxyLinks     <= minimum rest && muurCollisionEnemy (xEnemy - 1) yEnemy game && nOpposite ELeft           = ELeft
 | dxyUp        <= minimum rest && muurCollisionEnemy xEnemy (yEnemy + 1) game && nOpposite EUp             = EUp
 | dxyDown      <= minimum rest && muurCollisionEnemy xEnemy (yEnemy - 1) game && nOpposite EDown           = EDown
 | uncurry muurCollisionEnemy (gaZoDoor richtingEnemy) game                                                 = richtingEnemy
 | otherwise                                                                                                = undefined
 where  dxyRechts   = dxy (xEnemy + 1) yEnemy xDoel yDoel
        dxyLinks    = dxy (xEnemy - 1) yEnemy xDoel yDoel
        dxyUp       = dxy xEnemy (yEnemy + 1) xDoel yDoel
        dxyDown     = dxy xEnemy (yEnemy - 1) xDoel yDoel
        dxy x1 y1 x2 y2 = (x1 - x2)**2 + (y1 - y2)**2
        rest = case richtingEnemy of 
            ERight  -> [dxyRechts, dxyUp, dxyDown]
            ELeft   -> [dxyLinks, dxyUp, dxyDown]
            EUp     -> [dxyRechts, dxyLinks, dxyUp]
            EDown   -> [dxyRechts, dxyLinks, dxyDown]
        nOpposite enemyRichting
         | enemyRichting == richtingEnemy = False
         | otherwise = True
        gaZoDoor huidigeRichting = case huidigeRichting of
            ERight  -> ((xEnemy + 1), yEnemy)
            ELeft   -> ((xEnemy - 1), yEnemy)
            EUp     -> (xEnemy, (yEnemy + 1))
            EDown   -> (xEnemy, (yEnemy - 1))
