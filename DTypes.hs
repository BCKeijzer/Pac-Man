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
        enemyRichting1 :: ERichting,
        enemyRichting2 :: ERichting,
        enemyRichting3 :: ERichting,
        enemyRichting4 :: ERichting,
        muurLocatie :: [Muur],
        levend :: Levend,
        punten :: Int,
        voedsel :: [Voedsel],
        aardbei :: [Aardbei],
        aardbeiModus :: AardbeiModus,
        gepauzeerd :: Paused,
        gewonnen :: Gewonnen
    }

data Richting = SpelerUp | SpelerDown | SpelerRight | SpelerLeft | SpelerStill
data ERichting = EUp | EDown | ERight | ELeft | EStill
data Muur = Muur Float Float Float Float
data Levend = NogLevend | Dood
data Paused = NietPaused | WelPaused
data Voedsel = Voedsel Float Float
data Aardbei = Aardbei Float Float
data AardbeiModus = AardbeiAan | AardbeiUit
data Gewonnen = Gewonnen | NogBezig

instance Eq ERichting where
    ERight == ERight = True
    ELeft == ELeft = True
    EDown == EDown = True
    EUp == EUp = True
    _ == _ = False

initialState :: GameState
initialState = Game 
    {
        spelerLocatie = (10,0),
        richting = SpelerStill,
        enemyLocatie1 = (10,10),
        enemyLocatie2 = (-10,-10),
        enemyLocatie3 = (10,-10),
        enemyLocatie4 = (-10,10),
        enemyRichting1 = EStill,
        enemyRichting2 = EStill,
        enemyRichting3 = EStill,
        enemyRichting4 = EStill,
        muurLocatie = muurLijst,
        levend = NogLevend,
        punten = 0,
        voedsel = [],
        aardbei = [],
        aardbeiModus = AardbeiUit,
        gepauzeerd = NietPaused,
        gewonnen = NogBezig
    }

muurLijst :: [Muur] -- ^ formule Muur: Muur xlengte ylengte +-(240 - afstandx vanaf border - halve xlengte) +-(240 - afstandy vanaf border - halve ylengte)
muurLijst = [
    mkBovenMuur, mkOnderMuur, mkRechterMuur, mkLinkerMuur,
    muurLinksOnder1H, muurLinksOnder2H, muurLinksOnder3H, muurLinksOnder4H, muurLinksOnder5H, muurLinksOnder6H, muurLinksOnder7H,
    muurLinksOnder1V, muurLinksOnder2V, muurLinksOnder3V, muurLinksOnder5V, muurLinksOnder6V,
    muurLinksBoven1H, muurLinksBoven2H, muurLinksBoven3H, muurLinksBoven4H, muurLinksBoven5H, muurLinksBoven6H, muurRechtsBoven7H,
    muurLinksBoven1V, muurLinksBoven2V, muurLinksBoven3V, muurLinksBoven4V, muurLinksBoven5V, muurLinksBoven6V,
    muurRechtsOnder1H, muurRechtsOnder2H, muurRechtsOnder3H, muurRechtsOnder4H, muurRechtsOnder5H, muurRechtsOnder6H,
    muurRechtsOnder1V, muurRechtsOnder2V, muurRechtsOnder3V, muurRechtsOnder4V, muurRechtsOnder5V, muurRechtsOnder6V,
    muurRechtsBoven1H, muurRechtsBoven2H, muurRechtsBoven3H, muurRechtsBoven4H, muurRechtsBoven5H, muurRechtsBoven6H,
    muurRechtsBoven1V, muurRechtsBoven2V, muurRechtsBoven3V, muurRechtsBoven5V, muurRechtsBoven6V
    ]
    where   mkBovenMuur = Muur (fromIntegral hoogteBreedte) 10 0 (fromIntegral hoogteBreedte/2)
            mkOnderMuur = Muur (fromIntegral hoogteBreedte) 10 0 (-fromIntegral hoogteBreedte/2)
            mkRechterMuur = Muur 10 (fromIntegral hoogteBreedte) (fromIntegral hoogteBreedte/2) 0
            mkLinkerMuur = Muur 10 (fromIntegral hoogteBreedte) (-fromIntegral hoogteBreedte/2) 0
            muurLinksOnder1H = Muur 144 24 (-(240 - 24 - 144 / 2)) (-(240 - 24 - 24 / 2))
            muurLinksOnder2H = Muur 96 24 (-(240 - 96 / 2)) (-(240 - 72 - 24 / 2))
            muurLinksOnder3H = Muur 96 24 (-(240 - 48 - 96 / 2)) (-(240 - 168 - 24 / 2))
            muurLinksOnder4H = Muur 96 24 (-(240 - 72 - 96 / 2)) (-(240 - 120 - 24 / 2))
            muurLinksOnder5H = Muur 120 24 (-(240 - 72 - 120 / 2)) (-(240 - 216 - 24 / 2))
            muurLinksOnder6H = Muur 48 24 (-(240 - 168 - 48 / 2)) (-(240 - 192 - 24 / 2))
            muurLinksOnder7H = Muur 24 24 (-(240 - 192 - 24 / 2)) (-(240 - 24 - 24 / 2))
            muurLinksOnder1V = Muur 24 48 (-(240 - 120 - 24 / 2)) (-(240 - 48 - 48 / 2))
            muurLinksOnder2V = Muur 24 96 (-(240 - 24 - 24 / 2)) (-(240 - 120 - 96 / 2))
            muurLinksOnder3V = Muur 24 96 (-(240 - 168 - 24 / 2)) (-(240 - 72 - 96 / 2))
            --muurLinksOnder4V = Muur 24 48 (-(240 - 240 - 24 / 2)) (-(240 - 48 / 2))
            muurLinksOnder5V = Muur 24 48 (-(240 - 216 - 24 / 2)) (-(240 - 72 - 48 / 2))
            muurLinksOnder6V = Muur 24 24 (-(240 - 216 - 24 / 2)) (-(240 - 144 - 24 / 2))
            muurRechtsBoven1H = Muur 144 24 (240 - 24 - 144 / 2) (240 - 24 - 24 / 2)
            muurRechtsBoven2H = Muur 96 24 (240 - 96 / 2) (240 - 72 - 24 / 2)
            muurRechtsBoven3H = Muur 96 24 (240 - 48 - 96 / 2) (240 - 168 - 24 / 2)
            muurRechtsBoven4H = Muur 96 24 (240 - 72 - 96 / 2) (240 - 120 - 24 / 2)
            muurRechtsBoven5H = Muur 120 24 (240 - 72 - 120 / 2) (240 - 216 - 24 / 2)
            muurRechtsBoven6H = Muur 72 24 (240 - 168 - 72 / 2) (240 - 192 - 24 / 2)
            muurRechtsBoven7H = Muur 24 24 (240 - 192 - 24 / 2) (240 - 24 - 24 / 2)
            muurRechtsBoven1V = Muur 24 48 (240 - 120 - 24 / 2) (240 - 48 - 48 / 2)
            muurRechtsBoven2V = Muur 24 96 (240 - 24 - 24 / 2) (240 - 120 - 96 / 2)
            muurRechtsBoven3V = Muur 24 96 (240 - 168 - 24 / 2) (240 - 72 - 96 / 2)
            --muurRechtsBoven4V = Muur 24 48 (240 - 240 - 24 / 2) (240 - 48 / 2)
            muurRechtsBoven5V = Muur 24 48 (240 - 216 - 24 / 2) (240 - 72 - 48 / 2)
            muurRechtsBoven6V = Muur 24 72 (240 - 216 - 24 / 2) (240 - 144 - 72 / 2)
            muurRechtsOnder1H = Muur 192 24 (240 - 24 - 192 / 2) (-(240 - 24 - 24 / 2))
            muurRechtsOnder2H = Muur 96 24 (240 - 96 / 2) (-(240 - 72 - 24 / 2))
            muurRechtsOnder3H = Muur 96 24 (240 - 48 - 96 / 2) (-(240 - 168 - 24 / 2))
            muurRechtsOnder4H = Muur 96 24 (240 - 72 - 96 / 2) (-(240 - 120 - 24 / 2))
            muurRechtsOnder5H = Muur 120 24 (240 - 72 - 120 / 2) (-(240 - 216 - 24 / 2))
            muurRechtsOnder6H = Muur 48 24 (240 - 168 - 48 / 2) (-(240 - 192 - 24 / 2))
            muurRechtsOnder1V = Muur 24 48 (240 - 120 - 24 / 2) (-(240 - 48 - 48 / 2))
            muurRechtsOnder2V = Muur 24 120 (240 - 24 - 24 / 2) (-(240 - 120 - 120 / 2))
            muurRechtsOnder3V = Muur 24 96 (240 - 168 - 24 / 2) (-(240 - 72 - 96 / 2))
            muurRechtsOnder4V = Muur 24 48 (240 - 240 - 24 / 2) (-(240 - 48 / 2))
            muurRechtsOnder5V = Muur 24 48 (240 - 216 - 24 / 2) (-(240 - 72 - 48 / 2))
            muurRechtsOnder6V = Muur 24 24 (240 - 216 - 24 / 2) (-(240 - 144 - 24 / 2))
            muurLinksBoven1H = Muur 192 24 (-(240 - 24 - 192 / 2)) (240 - 24 - 24 / 2)
            muurLinksBoven2H = Muur 96 24 (-(240 - 96 / 2)) (240 - 72 - 24 / 2)
            muurLinksBoven3H = Muur 96 24 (-(240 - 48 - 96 / 2)) (240 - 168 - 24 / 2)
            muurLinksBoven4H = Muur 96 24 (-(240 - 72 - 96 / 2)) (240 - 120 - 24 / 2)
            muurLinksBoven5H = Muur 120 24 (-(240 - 72 - 120 / 2)) (240 - 216 - 24 / 2)
            muurLinksBoven6H = Muur 72 24 (-(240 - 168 - 72 / 2)) (240 - 192 - 24 / 2)
            muurLinksBoven1V = Muur 24 48 (-(240 - 120 - 24 / 2)) (240 - 48 - 48 / 2)
            muurLinksBoven2V = Muur 24 120 (-(240 - 24 - 24 / 2)) (240 - 120 - 120 / 2)
            muurLinksBoven3V = Muur 24 96 (-(240 - 168 - 24 / 2)) (240 - 72 - 96 / 2)
            muurLinksBoven4V = Muur 24 48 (-(240 - 240 - 24 / 2)) (240 - 48 / 2)
            muurLinksBoven5V = Muur 24 48 (-(240 - 216 - 24 / 2)) (240 - 72 - 48 / 2)
            muurLinksBoven6V = Muur 24 72 (-(240 - 216 - 24 / 2)) (240 - 144 - 72 / 2)
hoogteBreedte, offset, fps :: Int
hoogteBreedte = 490
offset = 100
fps = 60