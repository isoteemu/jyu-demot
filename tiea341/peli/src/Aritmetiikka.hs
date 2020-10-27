module Aritmetiikka where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point

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
        x' = a + x
        y' = b + y

        pisteiden_kulma = (atan2 y' x')
        -- kulma' = (atan2 y x)

        kulma
            | pisteiden_kulma < 0 = pisteiden_kulma + pi*2
            | otherwise = pisteiden_kulma


-- | Laske kahden vektorin etäisyyden kulma
vektorienKulma :: Vector -> Vector -> Float
vektorienKulma a b = vektorien_kulma
    where
        vektorien_kulma = pisteKulma (pisteVektorista a) (pisteVektorista b)


-- | Lisää tehoa liike-vektoriin aikakertoimen verran.
liikeVektori :: Vector -> Vector -> Float -> Vector
liikeVektori liike (p_kulma, p_teho) aika = (
        kulma,
        etäisyys
    )
    where
        skaalattu_teho = (p_kulma, (p_teho * aika))

        kulma = vektorienKulma liike skaalattu_teho
        etäisyys = vektoriLäheisyys liike skaalattu_teho



-- | Apufunktio vektorien yhteenlaskuun
(#+) :: Vector -> Vector -> Vector
(a,b) #+ (x,y) = (a+x, b+y)
