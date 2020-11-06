module Lehmä where

import Graphics.Gloss.Data.Picture

import Peli
import Aritmetiikka
import Assets
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Data.List (partition)
import Graphics.Gloss.Data.Vector
import Graphics.Gloss


lehmä_mitat :: (Point, Point)
lehmä_mitat = ((0, 0), (63, 42))

data Lehmä = Lehmä {
    lehmä_sijainti  :: Point,
    lehmä_liike     :: Vector,

    lehmä_ai        :: LehmänAI
} deriving (Show)

-- | Lehmän AI tyyppi. Ei ole kuitenkaan jaksettu toteuttaa.
data LehmänAI = AktiiviLehmä {
    lehmä_suunta    :: Suunta, -- ^ Mihin suuntaan lehmä vaeltaa.
    lehmä_vaellus   :: Float   -- ^ Halu vaeltaa.
} | PassiiviLehmä deriving Show



data Suunta = Oikea | Vasen deriving (Show)


lehmäSprite :: Picture
lehmäSprite = translate 0 (korkeus' lehmä_mitat) $ Pictures [
            takaraaja [(8.32504, 40.32504), (12.32504, 40.32504), (12.32504, 26.32504), (8.32504, 26.32504), (8.32504, 40.32504)] , -- Takataka
            häntä [(4.32504, 6.32504), (0.32504000000000044, 6.32504), (0.32504000000000044, 24.32504), (2.3250400000000004, 24.32504), (2.3250400000000004, 8.325040000000001), (4.32504, 8.325040000000001), (4.32504, 6.32504)] , -- Häntä
            takaraaja [(40.32504, 26.32504), (40.32504, 40.32504), (42.32504, 40.32504), (42.32504, 26.32504), (40.32504, 26.32504)] , -- Etutaka
            keho [(4.32504, 6.32504), (4.32504, 42.32504), (8.325040000000001, 42.32504), (8.325040000000001, 26.32504), (42.32504, 26.32504), (42.32504, 42.32504), (46.32504, 42.32504), (46.32504, 26.32504), (50.32504, 26.32504), (52.32504, 6.325040000000001), (4.32504, 6.32504)] , 
            utareet [(10.32504, 24.32504), (10.32504, 28.32504), (18.32504, 28.32504), (18.32504, 24.32504), (10.32504, 24.32504)] , -- Utareet
            täplä [(16.32504, 8.32504), (16.32504, 18.32504), (34.32504, 18.32504), (34.32504, 8.325040000000001), (16.32504, 8.32504)] , -- Täplä
            täplä [(8.32504, 8.32504), (12.32504, 8.32504), (12.32504, 14.32504), (8.32504, 14.32504), (8.32504, 8.32504)] , -- Täplä
            täplä [(6.32504, 18.32504), (12.325040000000001, 18.32504), (12.325040000000001, 22.32504), (6.325040000000001, 22.32504), (6.32504, 18.32504)] , -- Täplä
            täplä [(38.32504, 18.32504), (38.32504, 24.32504), (44.32504, 24.32504), (44.32504, 18.32504), (38.32504, 18.32504)] , -- Täplä
            keho [(58.32504, 0.32504), (62.32504, 0.32504), (60.32504, 2.32504), (58.32504, 2.32504), (58.32504, 0.32504)] , -- Korva
            keho [(46.32504, 0.32504), (42.32504, 0.32504), (44.32504, 2.32504), (46.32504, 2.32504), (46.32504, 0.32504)] , -- Korva
            keho [(48.32504, 14.32504), (56.32504, 14.32504), (58.32504, 0.32503999999999955), (46.32504, 0.32503999999999955), (48.32504, 14.32504)] , -- Kallo
            -- TUNTEMATON CubicBezier(start=(54.32504+12.32504j), control1=(54.32504+12.32504j), control2=(54.32504+14.32504j), end=(54.32504+14.32504j))
            nenä [(50.32504, 14.32504), (50.32504, 12.32504), (54.32504, 14.32504), (50.32504, 14.32504)] , -- Nenä
            silmä [(48.32504, 4.32504), (48.32504, 8.325040000000001), (50.32504, 8.325040000000001), (50.32504, 4.325040000000001), (48.32504, 4.32504)] , -- Silmä
            silmä [(54.32504, 4.32504), (54.32504, 8.325040000000001), (56.32504, 8.325040000000001), (56.32504, 4.325040000000001), (54.32504, 4.32504)] -- Silmä

        ]
    where
        keho = color white . p
        silmä = color black . p
        häntä = color black . p
        takaraaja = color (greyN 0.3) . p
        täplä = color black . p
        nenä = color black . p
        utareet = color (makeColor 0.9 0.5 0.35 1) . p
        p = piirräSprite


-- -- | TODO: Luo lehmiä Navettojen sijainteihin
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

-- | Piirretään lehmiä.
piirräLehmät :: [Lehmä] -> Picture
piirräLehmät lehmät = Pictures (map piirräLehmä lehmät)

piirräLehmä :: Lehmä -> Picture
piirräLehmä lehmä = let
        (x, y) = (lehmä_sijainti lehmä)

        -- Lehmä on aina horistontin kanssa linjassa.
        kulma = -(radToDeg  $ fst $ vektoriPisteestä (x,y)) + 90
    in 
        translate x y $ rotate kulma $ lehmäSprite


osuukoLehmään :: (Point,Point) -> Lehmä -> Bool
osuukoLehmään (a,b) lehmä =
                            not (segClearsBox a b x y)
                        where
                            (x,y)  = lehmänTörmäysviiva lehmä


-- | Imee säteeseen osuvia lehmiä
imeLehmiä :: Point      -- ^ Piste maailmassa johon kohti imetään
          -> Float      -- ^ Säteen kulma jota kohden osuminen testataan
          -> Float      -- ^ Aikakerroin
          -> [Lehmä]
          -> [Lehmä]
imeLehmiä sijainti kulma aika lehmät =
    let
        säde_alapiste   = rotateV (kulma + pi/2) (0, 200)
        b               = sijainti #+ säde_alapiste
        (osuu, ei_osu)  = partition (osuukoLehmään (sijainti, b)) lehmät
    in
        (map (nostataLehmää sijainti aika) osuu) <> ei_osu


-- | Lisää lehmällä liikevektori kohti pistettä
nostataLehmää :: Point -> Float -> Lehmä -> Lehmä
nostataLehmää kohden aika lehmä =
    let
        k     = pisteKulma kohden (lehmä_sijainti lehmä)
        voima = lisääLiike (lehmä_liike lehmä) (k, painovoima + 2.0) aika
    in
        lehmä {lehmä_liike=voima}


päivitäLehmä :: Float -> Lehmä -> Lehmä
päivitäLehmä aika lehmä = lehmä {
                                lehmä_sijainti = sijainti,
                                lehmä_liike    = (kulma, voima * ilmanvastus)
                            }
    where
        (kulma, voima) = lisääPainovoima (lehmä_liike (lehmänAi aika lehmä)) (lehmä_sijainti lehmä) aika
        -- (kulma, voima)  = lehmä_liike lehmä
        sijainti = (lehmä_sijainti lehmä) #+ pisteVektorista (kulma, voima)


lehmänTörmäysviiva :: Lehmä -> (Point, Point)
lehmänTörmäysviiva navetta = 
                            let
                                sijainti = lehmä_sijainti navetta
                                a = sijainti #+ fst lehmä_mitat
                                b = sijainti #+ snd lehmä_mitat
                            in
                                (a, b)


-- | Lehmille on näennäinen AI mahdollista, muttei toteutettu.
lehmänAi :: Float -> Lehmä -> Lehmä
lehmänAi aika lehmä =
    let
        (suunta, liike) = vektoriPisteestä (lehmä_sijainti lehmä)
        lehmän_liike = case (lehmä_ai lehmä) of
            -- TODO: Lehmän liike.            
            AktiiviLehmä _ v -> lisääLiike (lehmä_liike lehmä) (suunta-pi/2, 0) aika
            PassiiviLehmä    -> lisääLiike (lehmä_liike lehmä) (suunta-pi/2, 0) aika
    in
        lehmä{lehmä_liike=lehmän_liike}
