module DTypes where

import Graphics.Gloss

data GameState = Game
    {
        spelerLocatie :: (Float,Float)
        richting :: Richting
        enemyLocatie1 :: (Float, Float)
        enemyLocatie2 :: (Float, Float)
        enemyLocatie3 :: (Float, Float)
        enemyLocatie4 :: (Float, Float)
        muurLocatie :: [Muren]
        levend :: Levend
        punten :: Int
        voedsel :: [Voedsel]
        aardbei :: [Aardbei]
        aardbeiModus :: AardbeiModus
        gepauzeerd :: Paused
        gewonnen :: Gewonnen
    } deriving (Show, Eq)

data Richting = Up | Down | Right | Left
data Muur = undefined
data Levend = NogLevend | Dood
data Paused = NietPaused | WelPaused
data Voedsel = (Float, Float)
data Aardbei = (Float, Float)
data AardbeiModus = AardbeiAan | AardbeiUit
data Gewonnen = Gewonnen | NogBezig

initialState :: GameState
initialState = Game 
    {
        spelerLocatie = undefined,
        richting = undefined,
        enemyLocatie1 = undefined,
        enemyLocatie2 = undefined,
        enemyLocatie3 = undefined,
        enemyLocatie4 = undefined,
        muurLocatie = undefined,
        levend = NogLevend,
        punten = 0,
        voedsel = undefined,
        aardbei = undefined,
        gepauzeerd = NietPaused,
        gewonnen = NogBezig
    }

hoogteBreedte, offset, fps :: Int
hoogteBreedte = 500
offset = 100
fps = 60