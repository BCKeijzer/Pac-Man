module Enemies where

import Collision
import Graphics.Gloss
import DTypes
import System.Random

moveEnemy1 :: GameState -> GameState
moveEnemy1 game = let   (xEnemy, yEnemy) = enemyLocatie1 game
                        (xPac, yPac) = spelerLocatie game 
                        richtingE1 = enemyRichting1 game in game {enemyRichting1 = (enemyDichtsteBij xEnemy yEnemy (xPac + 20) yPac richtingE1 game)}

moveEnemy2 :: GameState -> GameState
moveEnemy2 game = let   (xEnemy, yEnemy) = enemyLocatie2 game
                        (xPac, yPac) = spelerLocatie game 
                        richtingE2 = enemyRichting2 game in game {enemyRichting2 = (enemyDichtsteBij xEnemy yEnemy (xPac - 20) yPac richtingE2 game)}

moveEnemy3 :: GameState -> GameState
moveEnemy3 game = let   (xEnemy, yEnemy) = enemyLocatie3 game
                        (xPac, yPac) = spelerLocatie game 
                        richtingE3 = enemyRichting3 game in game {enemyRichting3 = (enemyDichtsteBij xEnemy yEnemy xPac (yPac + 20) richtingE3 game)}

moveEnemy4 :: GameState -> GameState
moveEnemy4 game = let   (xEnemy, yEnemy) = enemyLocatie4 game
                        (xPac, yPac) = spelerLocatie game 
                        richtingE4 = enemyRichting4 game in game {enemyRichting4 = (enemyDichtsteBij xEnemy yEnemy xPac (yPac - 20) richtingE4 game)}

enemyDichtsteBij :: Float -> Float -> Float -> Float -> ERichting -> GameState -> ERichting
enemyDichtsteBij xEnemy yEnemy xDoel yDoel richtingEnemy game
 | dxyRechts    <= minimum rest && muurCollisionEnemy (xEnemy + 1) yEnemy game && nOpposite ERight          = ERight
 | dxyLinks     <= minimum rest && muurCollisionEnemy (xEnemy - 1) yEnemy game && nOpposite ELeft           = ELeft
 | dxyUp        <= minimum rest && muurCollisionEnemy xEnemy (yEnemy + 1) game && nOpposite EUp             = EUp
 | dxyDown      <= minimum rest && muurCollisionEnemy xEnemy (yEnemy - 1) game && nOpposite EDown           = EDown
 | uncurry muurCollisionEnemy (gaZoDoor richtingEnemy) game                                                 = richtingEnemy
 | otherwise                                                                                                = kiesWillekeurigeRichting
 where  dxyRechts   = dxy (xEnemy + 1) yEnemy xDoel yDoel
        dxyLinks    = dxy (xEnemy - 1) yEnemy xDoel yDoel
        dxyUp       = dxy xEnemy (yEnemy + 1) xDoel yDoel
        dxyDown     = dxy xEnemy (yEnemy - 1) xDoel yDoel
        dxyStill    = dxy xEnemy yEnemy xDoel yDoel
        dxy x1 y1 x2 y2 = (x1 - x2)**2 + (y1 - y2)**2
        rest = case richtingEnemy of 
            ERight  -> [dxyRechts, dxyUp, dxyDown]
            ELeft   -> [dxyLinks, dxyUp, dxyDown]
            EUp     -> [dxyRechts, dxyLinks, dxyUp]
            EDown   -> [dxyRechts, dxyLinks, dxyDown]
        nOpposite enemyRichting
         | enemyRichting == tegenOvergestelde = False
         | otherwise = True
        tegenOvergestelde = case richtingEnemy of
            EUp -> EDown
            EDown -> EUp
            ERight -> ELeft
            ELeft -> ERight
        gaZoDoor huidigeRichting = case huidigeRichting of
            ERight  -> ((xEnemy + 1), yEnemy)
            ELeft   -> ((xEnemy - 1), yEnemy)
            EUp     -> (xEnemy, (yEnemy + 1))
            EDown   -> (xEnemy, (yEnemy - 1))
        kiesWillekeurigeRichting
         | muurCollisionEnemy xEnemy (yEnemy + 1) game      = EUp
         | muurCollisionEnemy (xEnemy + 1) yEnemy game      = ERight
         | muurCollisionEnemy xEnemy (yEnemy - 1) game      = EDown
         | muurCollisionEnemy (xEnemy - 1) yEnemy game      = ELeft