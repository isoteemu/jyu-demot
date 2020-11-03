{-#LANGUAGE DeriveAnyClass#-}
module Lehmä where

import Graphics.Gloss.Data.Picture

import Peli
import Aritmetiikka
import Assets
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Data.List (partition)
import Graphics.Gloss.Data.Vector


lehmä_mitat :: (Point, Point)
lehmä_mitat = ((0, 0), (63, 42))

data Lehmä = Lehmä {
    lehmä_sijainti  :: Point,
    lehmä_liike     :: Vector,

    lehmä_ai        :: LehmänAI
} deriving (Show)

data LehmänAI = AktiiviLehmä {
    lehmä_suunta    :: Suunta, -- ^ Mihin suuntaan lehmä vaeltaa.
    lehmä_vaellus   :: Float   -- ^ Halu vaeltaa.
} | PassiiviLehmä deriving Show

data Suunta = Oikea | Vasen deriving (Show)

-- (*) Oikea a = abs a
-- (*) Vasen a = negate $ abs a

-- -- | Luo lehmiä Navettojen sijainteihin
luoLehmiä :: Int -> [Lehmä]
luoLehmiä määrä = map prlg (take määrä sattuma)

-- | Luo pseudosatunnainen lehmä
prlg :: Float -> Lehmä
prlg i = let
        ai
            | i >= 0.5 = AktiiviLehmä {
                            lehmä_suunta  = Vasen,
                            lehmä_vaellus = i*2
                        }
            | otherwise = PassiiviLehmä

    in Lehmä {
        lehmä_sijainti  = head' $ koordinaatiKaarella [(maanpinnan_taso * 2 * i, 0)],
        lehmä_liike     = (0, 0),
        lehmä_ai        = ai
    }

-- | Piirretään puita.
piirräLehmät :: [Lehmä] -> Picture
piirräLehmät lehmät = Pictures (map piirräLehmä lehmät)

piirräLehmä :: Lehmä -> Picture
piirräLehmä lehmä = let
        (x, y) = (lehmä_sijainti lehmä)
        kulma = -(radToDeg  $ fst $ vektoriPisteestä (x,y)) + 90
    in 
        translate x y $ rotate kulma $ lehmäSprite


-- osuukoLehmään :: (Point,Point) -> [Lehmä] -> Bool
-- osuukoLehmään törmäysviivat lehmät = 
--         not $ and $ map (osuuko) lehmät
--     where
--         osuuko lehmä =
--             let
--                 (ala1,ala2) = törmäysviivat
--                 (va,oy)     = lehmänTörmäysviiva lehmä
--             in segClearsBox ala1 ala2 va oy

osuukoLehmään :: (Point,Point) -> Lehmä -> Bool
osuukoLehmään (a,b) lehmä = not (segClearsBox a b x y)
    where 
        (x,y) = lehmänTörmäysviiva lehmä

-- | Imee säteeseen osuvia lehmiä
imeLehmiä :: Point -> Float -> [Lehmä] -> [Lehmä]
imeLehmiä sijainti kulma lehmät = let
        b = sijainti #+ (rotateV (kulma + pi/2) (0, 200))
        (osuu,ei_osu) = partition (osuukoLehmään (sijainti, b)) lehmät

        nostataLehmää :: Point -> Lehmä -> Lehmä
        nostataLehmää kohden lehmä = lehmä{lehmä_liike=voima}
            where
                k = pisteKulma (lehmä_sijainti lehmä) kohden
                voima = trace ("K: " <> show (lehmä_sijainti lehmä) <> " " <> show kohden) lisääLiike (lehmä_liike lehmä) (k, 10) 1

    in map (nostataLehmää sijainti) osuu <> ei_osu


-- päivitäLehmä :: Float -> Lehmä -> Lehmä
-- päivitäLehmä aika lehmä =
--     let
--         harkinnut_lehmä = lehmänAi aika lehmä

--         -- Laske lehmän tahtoma sijainti
--         h_liike    = lehmä_liike harkinnut_lehmä
--         h_sijainti = (lehmä_sijainti lehmä) #+ (pisteVektorista (lehmä_liike harkinnut_lehmä))

--         -- Laske painovoiman vaikutus
--         liike    = lisääPainovoima h_liike h_sijainti aika
--         sijainti = h_sijainti #+ (pisteVektorista liike)
--     in
--         harkinnut_lehmä{lehmä_sijainti = sijainti, lehmä_liike=liike}


päivitäLehmä :: Float -> Lehmä -> Lehmä
päivitäLehmä aika lehmä = lehmä{lehmä_sijainti=sijainti, lehmä_liike=(kulma, voima*(0.99*aika))}
    where
        (kulma, voima) = lisääPainovoima (lehmä_liike lehmä) (lehmä_sijainti lehmä) aika
        sijainti = (lehmä_sijainti lehmä) #+ pisteVektorista (kulma, voima)

lehmänTörmäysviiva :: Lehmä -> (Point, Point)
lehmänTörmäysviiva navetta = 
                            let
                                sijainti = lehmä_sijainti navetta
                                a = sijainti #+ fst lehmä_mitat
                                b = sijainti #+ snd lehmä_mitat
                            in
                                (a, b)




lehmänAi :: Float -> Lehmä -> Lehmä
lehmänAi aika lehmä =
    let
        (suunta, liike) = vektoriPisteestä (lehmä_sijainti lehmä)
        lehmän_liike = case (lehmä_ai lehmä) of
            -- TODO: Lehmän liike.
            AktiiviLehmä _ v -> lisääLiike (lehmä_liike lehmä) (suunta-pi/2, 1) aika
            PassiiviLehmä    -> lisääLiike (lehmä_liike lehmä) (suunta-pi/2, 0) aika
    in
        lehmä{lehmä_liike=lehmän_liike}
