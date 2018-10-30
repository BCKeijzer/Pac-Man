module Step where

import Collision
import Enemies
import DTypes
import Graphics.Gloss

update :: Float -> GameState -> GameState
update seconden game = case gepauzeerd game of
    WelPaused -> game
    NietPaused -> moveEnemies $ verplaatsSpeler $ verplaatsSpelerMain game

moveEnemies :: GameState -> GameState
moveEnemies game = verplaatsEnemies $ moveEnemy1 $ moveEnemy2 $ moveEnemy3 $ moveEnemy4 game

verplaatsEnemies :: GameState -> GameState
verplaatsEnemies game = verplaatsEnemy1 $ verplaatsEnemy2 $ verplaatsEnemy3 $ verplaatsEnemy4 game

verplaatsEnemy1 :: GameState -> GameState
verplaatsEnemy1 game = game {enemyLocatie1 = verplaatsEnemyMain (enemyRichting1 game) (enemyLocatie1 game)}

verplaatsEnemy2 :: GameState -> GameState
verplaatsEnemy2 game = game {enemyLocatie2 = verplaatsEnemyMain (enemyRichting2 game) (enemyLocatie2 game)}

verplaatsEnemy3 :: GameState -> GameState
verplaatsEnemy3 game = game {enemyLocatie3 = verplaatsEnemyMain (enemyRichting3 game) (enemyLocatie3 game)}

verplaatsEnemy4 :: GameState -> GameState
verplaatsEnemy4 game = game {enemyLocatie4 = verplaatsEnemyMain (enemyRichting4 game) (enemyLocatie4 game)}

verplaatsEnemyMain :: ERichting -> (Float, Float) -> (Float, Float)
verplaatsEnemyMain richtingE (xEnemy, yEnemy) = case richtingE of
    EUp -> (xEnemy, yEnemy + 1)
    EDown -> (xEnemy, yEnemy - 1)
    ERight -> (xEnemy + 1, yEnemy)
    ELeft -> (xEnemy - 1, yEnemy)

verplaatsSpelerMain :: GameState -> GameState    
verplaatsSpelerMain game = case mogelijkerichting game of
        MUp    |  mUp game    -> game {richting = SpelerUp, mogelijkerichting = MGeen}
        MUp    -> game
        MDown  |  mDown game  -> game {richting = SpelerDown, mogelijkerichting = MGeen}
        MDown  -> game
        MRight |  mRight game -> game {richting = SpelerRight, mogelijkerichting = MGeen}
        MRight -> game
        MLeft  |  mLeft game  -> game {richting = SpelerLeft, mogelijkerichting = MGeen}
        MLeft  -> game
        MGeen  -> game

mUp :: GameState -> Bool
mUp game  | muurCollision game2 = False
          | otherwise  = True
         where game2 = game {spelerLocatie = anderCoor (spelerLocatie game)}
               anderCoor (x,y) = (x, y+1)

mDown :: GameState -> Bool
mDown game | muurCollision game2 = False
           | otherwise  = True
         where game2 = game {spelerLocatie = anderCoor (spelerLocatie game)}
               anderCoor (x,y) = (x, y-1)

mRight :: GameState -> Bool
mRight game | muurCollision game2 = False
            | otherwise  = True
         where game2 = game {spelerLocatie = anderCoor (spelerLocatie game)}
               anderCoor (x,y) = (x+1, y)

mLeft :: GameState -> Bool
mLeft game | muurCollision game2 = False
           | otherwise  = True
         where game2 = game {spelerLocatie = anderCoor (spelerLocatie game)}
               anderCoor (x,y) = (x-1, y)


verplaatsSpeler :: GameState -> GameState
verplaatsSpeler game = case richting game of
    SpelerUp -> game {spelerLocatie = spelerUp}
        where   spelerUp
                    | muurCollision $ game {spelerLocatie = eenUp} = spelerLocatie game
                    | otherwise = eenUp
                eenUp = let (x,y) = spelerLocatie game in (x, y+1)

    SpelerDown -> game {spelerLocatie = spelerDown}
        where   spelerDown
                    | muurCollision $ game {spelerLocatie = eenDown} = spelerLocatie game
                    | otherwise = eenDown
                eenDown = let (x,y) = spelerLocatie game in (x, y-1)

    SpelerRight -> game {spelerLocatie = spelerRechts}
        where   spelerRechts
                    | muurCollision $ game {spelerLocatie = eenRechts} = spelerLocatie game
                    | otherwise = eenRechts
                eenRechts = let (x,y) = spelerLocatie game in (x+1, y)

    SpelerLeft -> game {spelerLocatie = spelerLinks}
        where   spelerLinks
                    | muurCollision $ game {spelerLocatie = eenLinks} = spelerLocatie game
                    | otherwise = eenLinks
                eenLinks = let (x,y) = spelerLocatie game in (x-1, y)

    SpelerStill -> game
