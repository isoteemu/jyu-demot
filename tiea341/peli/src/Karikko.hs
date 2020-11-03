module Karikko where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Line


import Peli
import Aritmetiikka
import Assets

data Karikko = Puu {este_sijainti::Float} deriving (Show)


puu_mitat :: (Point, Point)
puu_mitat = ((0, 0), (88, 143))

luoKarikkoja :: Int -> [Karikko]
luoKarikkoja määrä = map (\i -> Puu (i * maanpinnan_taso * 2)) (take määrä sattuma)

karikonTörmäysviiva :: Karikko -> (Point, Point)
karikonTörmäysviiva karikko = case karikko of 
    Puu x -> let
                juuri = (x, 0)
                -- Puut on aina ylöspäin
                latva = juuri #+ (0, korkeus' puu_mitat)
            in
                pisteet $ koordinaatiKaarella [juuri,latva]


törmääköKarikkoon :: ((Point,Point),(Point,Point)) -> [Karikko] -> Bool
törmääköKarikkoon törmäysviivat karikot = 
        not $ and $ map (törmääköYhteen) karikot
    where
        törmääköYhteen karikko = let
                                    ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
                                    (va,oy)                   = karikonTörmäysviiva karikko
                                 in
                                    (segClearsBox ala1 ala2 va oy) && (segClearsBox ylä1 ylä2 va oy)

-- | Piirretään puita.
-- | Ei ole vaivauduttu yleistämään.
piirräKarikot :: [Karikko] -> Picture
piirräKarikot karikot =
        Pictures (map piirrä_karikko karikot)
    where
        piirrä_karikko karikko = case karikko of
            Puu x -> piirräMaanpinnalle (x, puun_korkeus) puuSprite


