module Aritmetiikka where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point

import Data.Fixed
import Peli

-- | Laske XY koordinaatit vektorille
pisteVektorista :: Vector -> Point
pisteVektorista (kulma, voima) = (x, y)
    where
        x = voima * cos( kulma )
        y = voima * sin( kulma )


-- | Muunna XY koordinaatit vektoriksi
vektoriPisteestä :: Point -> Vector
vektoriPisteestä (x, y) = (kulma, voima)
    where
        kulma = atan2 y x
        voima = sqrt (x*x + y*y)


-- | Laske kahden vektorin yhteenlaskettu pituus.
vektoriLäheisyys :: Vector -> Vector -> Float
vektoriLäheisyys a b = sqrt (x*x + y*y)
    where
        (a_x, a_y) = pisteVektorista a 
        (b_x, b_y) = pisteVektorista b
        x = a_x + b_x
        y = a_y + b_y

-- | Laske kahden vektorin etäisyys
vektoriEtäisyys :: Vector -> Vector -> Float
vektoriEtäisyys a b = sqrt (x*x + y*y)
    where
        (a_x, a_y) = pisteVektorista a 
        (b_x, b_y) = pisteVektorista b
        x = a_x - b_x
        y = a_y - b_y


-- | Laske kahden pisteen etäisyys
pisteEtäisyys :: Point -> Point -> Float
pisteEtäisyys (a,b) (x,y) =
        sqrt (w*w + h*h)
    where
        w = a - x
        h = b - y


-- | Laske kahden pisteen välinen kulma (atan2)
pisteKulma :: Point -> Point -> Float
pisteKulma (a,b) (x,y) =
        kulma
    where
        x' = a - x
        y' = b - y

        pisteiden_kulma = (atan2 y' x')
        -- kulma' = (atan2 y x)

        kulma
            | pisteiden_kulma < 0 = pi*2 - pisteiden_kulma 
            | otherwise = pisteiden_kulma


-- | Laske kahden vektorin etäisyyden kulma
vektorienKulma :: Vector -> Vector -> Float
vektorienKulma a b = vektorien_kulma
--vektorienKulma (a,_) (b,_) = vektorien_kulma
    where
        vektorien_kulma = pisteKulma (pisteVektorista a) (pisteVektorista b)
        -- vektorien_kulma = (a + (a - b)) `mod'` (pi * 2)


-- | Lisää tehoa liike-vektoriin aikakertoimen verran.
lisääLiike :: Vector -> Vector -> Float -> Vector
lisääLiike liike (p_kulma, p_teho) aika = (
        kulma,
        etäisyys
    )
    where
        (a,_) = liike
        -- KULMAN LASKUSSA ON ONGELMA
        skaalattu_teho = ((a + ((p_kulma - a) * aika)) `mod'` (pi * 2), (p_teho * aika))

        kulma = vektorienKulma liike skaalattu_teho
        etäisyys = vektoriLäheisyys liike skaalattu_teho


-- | Lisää painovoimaa
lisääPainovoima :: Vector -> Point -> Float -> Vector
lisääPainovoima liike sijainti aika 
        | etäisyys > 0  = uusi_voima
        | otherwise     = lisääLiike uusi_voima (kulma - pi, etäisyys) aika
    where
        kulma              = (pisteKulma (0,0) sijainti) + pi
        painovoima_vektori = (kulma, painovoima)

        uusi_voima    = lisääLiike liike painovoima_vektori aika
        uusi_sijainti = sijainti #+ (pisteVektorista uusi_voima)
        etäisyys      = etäisyysMaasta uusi_sijainti





-- | Palauta etäisyys maanpinnasta
etäisyysMaasta :: Point -> Float
etäisyysMaasta sijainti = (pisteEtäisyys (0,0) sijainti) - maanpinnan_taso


-- | Sijoita koordinaatit maanpinnan kaarelle.
--   -> Sijoitetaan maanpinnalle ja käännetään x:n verran.
koordinaatiKaarella :: [Point] -> [Point]
koordinaatiKaarella koordinaatit =
    let 
        -- Pienin koordinaatti on pivot piste, koska piirretyt assetit on tasattu 0-alkuisessa koordinaatistossa.
        pivot = (foldr (\a b -> min (fst a) b) x koordinaatit, foldr (\a b -> min (snd a) b) y koordinaatit)
            where
                (x, y) = head' koordinaatit

        -- Skaalaa vektorit suhteellisiksi
        skaalattu = map (\a -> a #- pivot ) koordinaatit

        -- Laske akseli, jonka kärki on uusi kierto piste
        kulma = (-(fst pivot) / (maanpinnan_taso*2)) * (pi*2)

        -- Monimutkaisempi mitä tarvitsisi, sillä 0-radiaani on on oikea reuna muttei rotateV:lle
        käännä v = rotateV (kulma - pi/2) v 
        akseli = käännä (0,maanpinnan_taso)
    in
        -- Kierrä suhteellisia pisteitä akselin kärjen sijainnissa.
        map (\piste -> akseli #+ (käännä piste)) skaalattu


-- | Apufunktio vektorien yhteenlaskuun
(#+) :: Vector -> Vector -> Vector
(a,b) #+ (x,y) = (a+x, b+y)

-- | Apufunktio vektorien yhteenlaskuun
(#-) :: Vector -> Vector -> Vector
(a,b) #- (x,y) = (a-x, b-y)


-- | Satunnaistaulukko. Suoraan alkuperäisen doomin määrittelemänä.
satunnaisTaulukko :: Num a => [a]
satunnaisTaulukko = cycle [
    0,   8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66 ,
    74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36 ,
    95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188 ,
    52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224 ,
    149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242 ,
    145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0 ,
    175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235 ,
    25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113 ,
    94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75 ,
    136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196 ,
    135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113 ,
    80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241 ,
    24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224 ,
    145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95 ,
    28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226 ,
    71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36 ,
    17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106 ,
    197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136 ,
    120, 163, 236, 249 ]


-- | Palauttaa listan satunnaisia lukuja 0 - 1 asteikolla.
sattuma :: Fractional a => [a]
sattuma = map (/255) satunnaisTaulukko
