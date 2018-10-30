module Enemies where

import Collision
import Graphics.Gloss
import DTypes
import System.Random

data TeDichtBij = DichtBij | Verweg

moveEnemy1 :: GameState -> GameState
moveEnemy1 game
 | enemyMode game == Chase      = let   (xEnemy, yEnemy) = enemyLocatie1 game
                                        (xPac, yPac) = spelerLocatie game 
                                        richtingE1 = enemyRichting1 game in 
                                            case teDichtBij (dxy xEnemy yEnemy xPac yPac) of
                                                DichtBij -> game {enemyRichting1 = (enemyDichtsteBij xEnemy yEnemy xPac yPac richtingE1 game)}
                                                Verweg -> game {enemyRichting1 = (enemyDichtsteBij xEnemy yEnemy (xPac + 50) yPac richtingE1 game)}
 | enemyMode game == Scatter    = let   (xEnemy, yEnemy) = enemyLocatie1 game
                                        (xScatter, yScatter) = scatterLocatie 1 
                                        richtingE1 = enemyRichting1 game in game {enemyRichting1 = (enemyDichtsteBij xEnemy yEnemy xScatter yScatter richtingE1 game)}
 | otherwise                    = game { enemyRichting1 = probeerNaarBeneden (enemyLocatie1 game) game }   

moveEnemy2 :: GameState -> GameState
moveEnemy2 game
 | enemyMode game == Chase      = let   (xEnemy, yEnemy) = enemyLocatie2 game
                                        (xPac, yPac) = spelerLocatie game 
                                        richtingE2 = enemyRichting2 game in
                                            case teDichtBij (dxy xEnemy yEnemy xPac yPac) of
                                                DichtBij -> game {enemyRichting2 = (enemyDichtsteBij xEnemy yEnemy xPac yPac richtingE2 game)}
                                                Verweg -> game {enemyRichting2 = (enemyDichtsteBij xEnemy yEnemy (xPac - 50) yPac richtingE2 game)}
 | enemyMode game == Scatter    = let   (xEnemy, yEnemy) = enemyLocatie2 game
                                        (xScatter, yScatter) = scatterLocatie 2 
                                        richtingE2 = enemyRichting2 game in game {enemyRichting2 = (enemyDichtsteBij xEnemy yEnemy xScatter yScatter richtingE2 game)}
 | otherwise                    = game { enemyRichting2 = probeerNaarBeneden (enemyLocatie2 game) game }        

moveEnemy3 :: GameState -> GameState
moveEnemy3 game
 | enemyMode game == Chase      = let   (xEnemy, yEnemy) = enemyLocatie3 game
                                        (xPac, yPac) = spelerLocatie game 
                                        richtingE3 = enemyRichting3 game in
                                            case teDichtBij (dxy xEnemy yEnemy xPac yPac) of
                                                DichtBij -> game {enemyRichting3 = (enemyDichtsteBij xEnemy yEnemy xPac yPac richtingE3 game)}
                                                Verweg ->  game {enemyRichting3 = (enemyDichtsteBij xEnemy yEnemy xPac (yPac + 50) richtingE3 game)}
 | enemyMode game == Scatter    = let   (xEnemy, yEnemy) = enemyLocatie3 game
                                        (xScatter, yScatter) = scatterLocatie 3 
                                        richtingE3 = enemyRichting3 game in game {enemyRichting3 = (enemyDichtsteBij xEnemy yEnemy xScatter yScatter richtingE3 game)}
 | otherwise                    = game { enemyRichting3 = probeerNaarBeneden (enemyLocatie3 game) game }   

moveEnemy4 :: GameState -> GameState
moveEnemy4 game
 | enemyMode game == Chase      = let   (xEnemy, yEnemy) = enemyLocatie4 game
                                        (xPac, yPac) = spelerLocatie game 
                                        richtingE4 = enemyRichting4 game in
                                            case teDichtBij (dxy xEnemy yEnemy xPac yPac) of
                                                DichtBij -> game {enemyRichting4 = (enemyDichtsteBij xEnemy yEnemy xPac yPac richtingE4 game)}
                                                Verweg -> game {enemyRichting4 = (enemyDichtsteBij xEnemy yEnemy xPac (yPac - 50) richtingE4 game)}
 | enemyMode game == Scatter    = let   (xEnemy, yEnemy) = enemyLocatie4 game
                                        (xScatter, yScatter) = scatterLocatie 4 
                                        richtingE4 = enemyRichting4 game in game {enemyRichting4 = (enemyDichtsteBij xEnemy yEnemy xScatter yScatter richtingE4 game)}
 | otherwise                    = game { enemyRichting4 = probeerNaarBeneden (enemyLocatie4 game) game }   

enemyDichtsteBij :: Float -> Float -> Float -> Float -> ERichting -> GameState -> ERichting
enemyDichtsteBij xEnemy yEnemy xDoel yDoel richtingEnemy game
 | dxyRechts    <= minimum rest && muurCollisionEnemy (xEnemy + 1) yEnemy game && nOpposite ERight          = ERight
 | dxyLinks     <= minimum rest && muurCollisionEnemy (xEnemy - 1) yEnemy game && nOpposite ELeft           = ELeft
 | dxyUp        <= minimum rest && muurCollisionEnemy xEnemy (yEnemy + 1) game && nOpposite EUp             = EUp
 | dxyDown      <= minimum rest && muurCollisionEnemy xEnemy (yEnemy - 1) game && nOpposite EDown           = EDown
 | uncurry muurCollisionEnemy (gaZoDoor richtingEnemy) game                                                 = richtingEnemy
 | otherwise                                                                                                = kiesWillekeurigeRichting
 where  dxyRechts   = dxy (xEnemy + 5) yEnemy xDoel yDoel
        dxyLinks    = dxy (xEnemy - 5) yEnemy xDoel yDoel
        dxyUp       = dxy xEnemy (yEnemy + 5) xDoel yDoel
        dxyDown     = dxy xEnemy (yEnemy - 5) xDoel yDoel
        dxyStill    = dxy xEnemy yEnemy xDoel yDoel
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
         | otherwise                                        = EUp

dxy :: Float -> Float -> Float -> Float -> Float
dxy x1 y1 x2 y2 = (x1 - x2)**2 + (y1 - y2)**2

teDichtBij :: Float -> TeDichtBij
teDichtBij afstand
 | afstand < 2500       = DichtBij
 | otherwise            = Verweg

probeerNaarBeneden :: (Float, Float) -> GameState -> ERichting
probeerNaarBeneden (x, y) game
 | muurCollisionEnemy x (y-1) game          = EDown
 | muurCollisionEnemy (x+1) y game          = ERight
 | muurCollisionEnemy (x-1) y game          = ELeft