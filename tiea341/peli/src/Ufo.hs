module Ufo where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Data.Fixed
import Aritmetiikka
import Peli
import Graphics.Gloss

data Ufo = Ufo {
    ufo_sijainti    :: Point,
    ufo_teho        :: Vector, -- ^ Tehovektori (kulma,voima)
    ufo_liike       :: Vector,  -- ^ Tehovektori (kulma,voima)
    ufo_traktori    :: Traktori
} deriving (Show)

data Traktori = Päällä | Pois deriving (Show)


ufo_mitat :: (Point,Point)
ufo_mitat = ((-60, 0), (60, -45))


lautanen :: Float -> Picture
lautanen aika = color väri (translate 0 korkeus keho)
    where
        -- Ufon paramtrejä
        väri        = greyN 0.9
        leveys      = leveys' ufo_mitat / 2
        korkeus     = korkeus' ufo_mitat / 2
        nopeus      = 3

        aika' = aika * 0.8

        lisääPallura offset
            | luku <= ositus || luku >= ositus * 3 = pallura
            | otherwise = blank
            where
                ositus = pi*2/4
                luku = (aika' * nopeus + offset) `mod'` (pi*2)
                skaala = cos(luku)
                sijainti = sin(luku) * (-1)
                pallura = translate (sijainti*leveys-sijainti) 0 (scale skaala 1 $ color blue (circleSolid (korkeus*0.3)))

        runko = (scale 1.0 0.4 (circleSolid leveys))
            <> translate 0 (korkeus * 0.85) (scale 1.0 0.6 (circleSolid korkeus)) -- Ohjaamo
            <> translate 0 (korkeus * (-0.9)) (scale 1.0 0.2 ( -- Imutusreikä
                circleSolid korkeus
                <> (color (light yellow) (circleSolid (korkeus*0.9)))
            ))
        -- Lisää pyörivät pallurat
        keho = foldr (\n runko' -> runko' <> lisääPallura (pi*2/6*n)) runko [1..6]


traktoriSprite :: Picture
-- #94fafd
traktoriSprite =
        Pictures [
            color väri $ rectangleSolid leveys pituus,
            color (light väri) $ rectangleSolid (leveys * 0.3) pituus,
            color (light väri) $  line [(leveys*0.2,-pituus*0.5),(leveys*0.5,pituus*0.5)]
                               <> line [(-leveys*0.2,-pituus*0.5),(-leveys*0.5,pituus*0.5)]
        ]
    where
        pituus     = 200
        leveys     = leveys' ufo_mitat * 0.3
        väri       = makeColor 0.6 0.9 0.95 0.6



-- | Tuottaa ufon törmäykseen käytettävät pisteet.
--   Eroaa esimerkistä käyttämällä radiaaneja.
ufoTörmäysviivat :: Ufo -> ((Point,Point) , (Point,Point))
ufoTörmäysviivat ufo =
    let
        paikka      = ufo_sijainti ufo
        kulma       = fst (ufo_teho ufo)

        ((vasen, ylä), (oikea,ala)) = ufo_mitat

        kääntö      = rotateV (kulma + pi/2)
    in (
            (
                kääntö (vasen, ylä) #+ paikka,
                kääntö (oikea, ylä) #+ paikka
            ),
            (
                kääntö (vasen, ala) #+ paikka,
                kääntö (oikea, ala) #+ paikka
            )
       )

muutaTehoa :: Float -> Ufo -> Ufo
muutaTehoa teho_muutos hahmo = hahmo { 
                                    ufo_teho = (ufo_teho hahmo) #+ (0,teho_muutos)
                                }


kallista :: Float -> Ufo -> Ufo
kallista kulma_muutos hahmo = hahmo {
                                    ufo_teho = (ufo_teho hahmo) #+ (kulma_muutos,0)
                                }

traktori :: Traktori -> Ufo -> Ufo
traktori tila ufo = ufo{ufo_traktori=tila}
