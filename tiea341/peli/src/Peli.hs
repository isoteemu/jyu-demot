module Peli where
{-
    Apufunktioita ja vakioita
-}

import Graphics.Gloss.Data.Picture
-- | Maanpinnan taso vakiona elämän helpottamiseksi
maanpinnan_taso :: Num p => p
maanpinnan_taso = 5000

painovoima :: Num p => p
painovoima = 4

ilmanvastus :: Fractional p => p
ilmanvastus = 0.98

-- | Piirrä maanpinnalle.
piirräMaanpinnalle :: Point -> Picture -> Picture
piirräMaanpinnalle (x, y) = rotate (360 * (x / (maanpinnan_taso * 2)) + 90) . translate (0) (maanpinnan_taso + y)

leveys' :: Num a => ((a, b), (a, b)) -> a
leveys' ((x1,_), (x2,_)) = abs x1 + abs x2

korkeus' :: Num a => ((b, a), (b, a)) -> a
korkeus' ((_,y1), (_,y2)) = abs y1 + abs y2

-- * Tilanteisiin jolloin tiedetään ettei käsitellä tyhjiä listoja.

head' :: [p] -> p
head' []    = error "Mahdoton tila"
head' (x:_) = x

-- | Palauta listan kaksi ekaa alkiota tuplena.
pisteet :: [Point] -> (Point,Point)
pisteet (x:y:_) = (x,y)
pisteet       _ = error "Shutup ghc"
