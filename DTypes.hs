module DTypes where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

data GameState = Game
    {
        spelerLocatie :: (Float,Float),
        mogelijkerichting :: MRichting,
        richting :: Richting,
        enemyMode :: EnemyMode,
        enemyLocatie1 :: (Float, Float),
        enemyLocatie2 :: (Float, Float),
        enemyLocatie3 :: (Float, Float),
        enemyLocatie4 :: (Float, Float),
        enemyRichting1 :: ERichting,
        enemyRichting2 :: ERichting,
        enemyRichting3 :: ERichting,
        enemyRichting4 :: ERichting,
        muurLocatie :: [Muur],
        veld :: [Field],
        levend :: Levend,
        punten :: Int,
        highscores :: String,
        voedsel :: [Voedsel],
        aardbei :: [Aardbei],
        aardbeiModus :: AardbeiModus,
        gepauzeerd :: Paused,
        gewonnen :: Gewonnen,
        tijd :: Int
    }

data Richting = SpelerUp | SpelerDown | SpelerRight | SpelerLeft | SpelerStill
data MRichting = MUp | MRight | MDown | MLeft | MGeen
data EnemyMode = Scatter | Chase | ModusDown
data ERichting = EUp | EDown | ERight | ELeft
data Muur = Muur Float Float Float Float
data Levend = NogLevend | Dood
data Field = LeegVeld | Wall (Float, Float) (Float, Float) | Item (Float, Float) (Float, Float)
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

initialState :: String -> GameState
initialState highscoreLijst = Game 
    {
        spelerLocatie = ((-12),(84)),
        mogelijkerichting = MGeen,
        richting = SpelerStill,
        enemyMode = ModusDown,
        enemyLocatie1 = (10,(-30)),
        enemyLocatie2 = (5,(-30)),
        enemyLocatie3 = ((-5),(-30)),
        enemyLocatie4 = ((-10),(-30)),
        enemyRichting1 = EUp,
        enemyRichting2 = EUp,
        enemyRichting3 = EUp,
        enemyRichting4 = EUp,
        muurLocatie = muurLijst,
        veld = veldLijst,
        levend = NogLevend,
        punten = 0,
        highscores = highscoreLijst,
        voedsel = [],
        aardbei = [],
        aardbeiModus = AardbeiUit,
        gepauzeerd = NietPaused,
        gewonnen = NogBezig,
        tijd = 0
    }


scatterLocatie :: Int -> (Float, Float)
scatterLocatie getal
 | getal == 1       = ((-240), (240))
 | getal == 2       = ((240),  (240))
 | getal == 3       = ((240),  (-240))
 | getal == 4       = ((-240), (-240))


instance Eq EnemyMode where
    Scatter == Scatter = True
    Chase == Chase = True
    _ == _ = False
--instance Eq Field where
--    Wall (x1,y1) (x2, y2)  == (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- | dit is het level, waar per blok weergegeven is of het een muur is (door een wall te maken),
-- | of het een item is (door een item te maken)
-- | 
veldLijst :: [Field]
veldLijst = concat [rij0, rij1, rij2, rij3, rij4, rij5, rij6, rij7, rij8, rij9, rij10, rij11,
                    rij12, rij13, rij14, rij15, rij16, rij17, rij18, rij19, rij20, rij21]
 where  rij0 = [mkWall ((-264), 264), mkWall ((-240), 264), mkWall ((-216), 264), mkWall ((-192), 264),
                mkWall ((-168), 264), mkWall ((-144), 264), mkWall ((-120), 264), mkWall ((-96), 264),
                mkWall ((-72), 264), mkWall ((-48), 264), mkWall ((-24), 264), mkWall (0, 264),
                mkWall (24,264), mkWall (48,264), mkWall (72,264), mkWall (96,264), mkWall (120,264),
                mkWall (144,264), mkWall (168,264), mkWall (192,264), mkWall (216,264),
                mkWall (240,264)]
        rij1 = [mkWall ((-264), 240), mkWall ((-72), 240), mkWall ((-24), 240), mkWall (120, 240)]
        rij2 = [mkWall ((-264), 216), mkWall ((-216), 216), mkWall ((-168), 216), mkWall ((-144), 216), mkWall((-120),216), mkWall ((-24), 216), mkWall (24, 216), mkWall (48, 216), mkWall (72,216), mkWall (168, 216), mkWall (192, 216), mkWall (240, 216), mkWall (240, 240)]
        rij3 = [mkWall ((-264), 192), mkWall ((-216), 192), mkWall ((-168), 192), mkWall ((-72), 192), mkWall ((-24), 192), mkWall (120, 192), mkWall (168, 192), mkWall (240, 192)]
        rij4 = [mkWall ((-264), 168), mkWall ((-120), 168), mkWall ((-96), 168), mkWall ((-72),168), mkWall (24, 168), mkWall (48, 168), mkWall (72,168),mkWall (168, 168), mkWall (216, 168), mkWall (240, 168)]
        rij5 = [mkWall ((-264), 144), mkWall ((-240), 144), mkWall ((-216),144), mkWall ((-192),144), mkWall ((-168),144), mkWall ((-120),144), mkWall ((-24), 144), mkWall (120, 144), mkWall (216, 144), mkWall (240, 144)]
        rij6 = [mkWall ((-264), 120), mkWall ((-48),120), mkWall ((-24),120), mkWall (0,120), mkWall (24, 120), mkWall (72, 120), mkWall (96, 120), mkWall (120, 120), mkWall (168, 120), mkWall (240, 120)]
        rij7 = [mkWall ((-264), 96), mkWall ((-216), 96), mkWall ((-168), 96), mkWall ((-144),96), mkWall ((-120),96), mkWall ((-96), 96), mkWall (0, 96), mkWall (168, 96), mkWall (192, 96), mkWall (240, 96)]
        rij8 = [mkWall ((-264), 72), mkWall ((-216), 72), mkWall ((-96), 72), mkWall ((-72), 72), mkWall ((-24), 72), mkWall (0, 72), mkWall (48, 72), mkWall (72,72), mkWall (96, 72), mkWall (120,72), mkWall (240, 72)]
        rij9 = [mkWall ((-264), 48), mkWall ((-216),48), mkWall ((-192),48), mkWall ((-168), 48), mkWall ((-144), 48), mkWall (96, 48), mkWall (240, 48)]
        rij10 = [mkWall ((-264), 24), mkWall ((-96), 24), mkWall ((-48),24), mkWall ((-24), 24), mkWall (0, 24), mkWall (24,24), mkWall (48,24), mkWall (240, 24)]
        rij11 = [mkWall ((-264), 0), mkWall ((-240), 0), mkWall ((-216), 0), mkWall ((-168), 0), mkWall ((-120), 0), mkWall ((-96),0), mkWall ((-48),0), mkWall (48, 0), mkWall (240, 0)]
        rij12 = [mkWall ((-264), (-24)), mkWall ((-96),(-24)), mkWall ((-48), (-24)), mkWall (48, (-24)), mkWall (240, (-24))]
        rij13 = [mkWall ((-264), (-48)), mkWall ((-216),(-48)), mkWall ((-192), (-48)), mkWall ((-144),(-48)), mkWall (240, (-48))]
        rij14 = [mkWall ((-264), (-72)), mkWall ((-216),(-72)), mkWall ((-144),(-72)), mkWall ((-120), (-72)), mkWall ((-96), (-72)), mkWall ((-72), (-72)), mkWall ((-24), (-72)), mkWall (0, (-72)), mkWall (240, (-72))]
        rij15 = [mkWall ((-264), (-96)), mkWall ((-216),(-96)), mkWall ((-192), (-96)), mkWall ((-24),(-96)), mkWall (96, (-96)), mkWall (120, (-96)), mkWall(144, (-96)), mkWall (192, (-96)), mkWall (216, (-96)), mkWall (240, (-96))]
        rij16 = [mkWall ((-264), (-120)), mkWall ((-144),(-120)), mkWall ((-96),(-120)), mkWall ((-48),(-120)), mkWall ((-24),(-120)), mkWall (240, (-120))]
        rij17 = [mkWall ((-264), (-144)), mkWall ((-216), (-144)), mkWall ((-168), (-144)), mkWall ((-96),(-144)), mkWall ((-144), (-144)), mkWall (72, (-144)), mkWall (120, (-144)), mkWall (144, (-144)), mkWall (168, (-144)), mkWall (192, (-144)), mkWall (240, (-144))]
        rij18 = [mkWall ((-264), (-168)), mkWall ((-216), (-168)), mkWall ((-144), (-168)), mkWall ((-96),(-168)), mkWall ((-48),(-168)), mkWall (72, (-168)), mkWall (240, (-168))]
        rij19 = [mkWall ((-264), (-192)), mkWall ((-216), (-192)), mkWall ((-192), (-192)), mkWall ((-96),(-192)), mkWall ((-48),(-192)), mkWall (72, (-192)), mkWall (120, (-192)), mkWall (144, (-192)), mkWall (168, (-192)), mkWall (192, (-192)), mkWall (240, (-192))]
        rij20 = [mkWall ((-264), (-216)), mkWall ((-144), (-216)), mkWall ((-48),(-216)), mkWall (240, (-216))]
        rij21 = [mkWall ((-264), (-240)), mkWall ((-240), (-240)), mkWall ((-216), (-240)), mkWall ((-192), (-240)),
                mkWall ((-168), (-240)), mkWall ((-144), (-240)), mkWall ((-120), (-240)), mkWall ((-96), (-240)),
                mkWall ((-72), (-240)), mkWall ((-48), (-240)), mkWall ((-24), (-240)), mkWall (0, (-240)),
                mkWall (24,(-240)), mkWall (48,(-240)), mkWall (72,(-240)), mkWall (96,(-240)), mkWall (120,(-240)),
                mkWall (144,(-240)), mkWall (168,(-240)), mkWall (192,(-240)), mkWall (216,(-240)), mkWall (240,(-240)),
                mkWall (240,(-240))]
        mkWall (x,y) = Wall (x, y) (x + 24, y - 24)

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