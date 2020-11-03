module Ufo where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Aritmetiikka

data Ufo = Ufo {
    ufo_sijainti    :: Point,
    ufo_teho        :: Vector, -- ^ Tehovektori (kulma,voima)
    ufo_liike       :: Vector,  -- ^ Tehovektori (kulma,voima)
    ufo_traktori    :: Traktori
} deriving (Show)

data Traktori = Päällä | Pois deriving (Show)


ufo_mitat :: (Point,Point)
ufo_mitat = ((-60, 0), (60, -45))

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
