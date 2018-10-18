module DTypes where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

data GameState = Game
    {
        spelerLocatie :: (Float,Float),
        richting :: Richting,
        enemyLocatie1 :: (Float, Float),
        enemyLocatie2 :: (Float, Float),
        enemyLocatie3 :: (Float, Float),
        enemyLocatie4 :: (Float, Float),
        muurLocatie :: [Muur],
        levend :: Levend,
        punten :: Int,
        voedsel :: [Voedsel],
        aardbei :: [Aardbei],
        aardbeiModus :: AardbeiModus,
        gepauzeerd :: Paused,
        gewonnen :: Gewonnen
    }

data Richting = SpelerUp | SpelerDown | SpelerRight | SpelerLeft
data Muur = Muur Float Float Float Float
data MuurOmvang = MuurOmvang Float Float
data MuurTranslate = MuurTranslate Float Float
data Levend = NogLevend | Dood
data Paused = NietPaused | WelPaused
data Voedsel = Voedsel Float Float
data Aardbei = Aardbei Float Float
data AardbeiModus = AardbeiAan | AardbeiUit
data Gewonnen = Gewonnen | NogBezig

initialState :: GameState
initialState = Game 
    {
        spelerLocatie = (0,0),
        richting = SpelerUp,
        enemyLocatie1 = (10,10),
        enemyLocatie2 = (-10,-10),
        enemyLocatie3 = (10,-10),
        enemyLocatie4 = (-10,10),
        muurLocatie = [(Muur 25 50 50 (-50)), (Muur 60 40 (-80) (-30))],
        levend = NogLevend,
        punten = 0,
        voedsel = [],
        aardbei = [],
        aardbeiModus = AardbeiUit,
        gepauzeerd = NietPaused,
        gewonnen = NogBezig
    }

hoogteBreedte, offset, fps :: Int
hoogteBreedte = 500
offset = 100
fps = 60