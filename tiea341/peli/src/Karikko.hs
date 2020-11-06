module Karikko where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Line


import Peli
import Aritmetiikka
import Assets
import Graphics.Gloss

data Karikko = Puu {este_sijainti::Float} deriving (Show)

puu_mitat :: (Point, Point)
puu_mitat = ((0, 0), (88, 143))


puuSprite :: Picture
puuSprite = translate (0 - leveys' puu_mitat / 2) 0 $ Pictures [
            color juuri $ p [(32.0, 122.0), (28.0, 134.0), (32.0, 136.0), (36.0, 142.0), (46.0, 142.0), (48.0, 140.0), (52.0, 140.0), (56.0, 136.0), (56.0, 130.0), (52.0, 124.0), (48.0, 108.0), (34.0, 108.0), (32.0, 122.0)], -- Juuri
            color havusto $ p [(40.0, 10.0), (32.0, 18.0), (38.0, 16.0), (32.0, 34.0), (22.0, 44.0), (26.0, 42.0), (22.0, 48.0), (28.0, 46.0), (10.0, 64.0), (26.0, 60.0), (8.0, 78.0), (12.0, 78.0), (6.0, 86.0), (14.0, 84.0), (12.0, 88.0), (16.0, 86.0), (6.0, 96.0), (12.0, 94.0), (0.0, 108.0), (10.0, 106.0), (6.0, 112.0), (24.0, 104.0), (18.0, 116.0), (28.0, 110.0), (26.0, 116.0), (36.0, 110.0), (40.0, 120.0), (44.0, 110.0), (60.0, 118.0), (58.0, 112.0), (64.0, 114.0), (62.0, 110.0), (68.0, 112.0), (68.0, 108.0), (86.0, 112.0), (68.0, 96.0), (80.0, 100.0), (72.0, 84.0), (80.0, 86.0), (72.0, 76.0), (80.0, 76.0), (66.0, 64.0), (76.0, 66.0), (72.0, 60.0), (78.0, 62.0), (62.0, 46.0), (70.0, 48.0), (56.0, 32.0), (54.0, 24.0), (60.0, 26.0), (54.0, 18.0), (60.0, 20.0), (50.0, 10.0), (46.0, 0.0), (40.0, 10.0)], -- Havusto
            color varjostus $ p [(38.0, 16.0), (42.0, 14.0), (42.0, 28.0), (48.0, 18.0), (54.0, 24.0), (54.0, 28.0), (48.0, 22.0), (42.0, 32.0), (38.0, 20.0), (36.0, 22.0), (38.0, 16.0)], -- Varjostus
            color varjostus $ p [(28.0, 46.0), (30.0, 44.0), (30.0, 50.0), (36.0, 46.0), (38.0, 56.0), (46.0, 44.0), (54.0, 52.0), (52.0, 40.0), (62.0, 50.0), (62.0, 46.0), (66.0, 56.0), (56.0, 50.0), (56.0, 58.0), (46.0, 48.0), (38.0, 60.0), (34.0, 50.0), (26.0, 54.0), (28.0, 48.0), (24.0, 50.0), (28.0, 46.0)], -- Varjostus
            color varjostus $ p [(26.0, 60.0), (30.0, 58.0), (28.0, 70.0), (28.0, 62.0), (22.0, 64.0), (26.0, 60.0)], -- Varjostus
            color varjostus $ p [(40.0, 64.0), (28.0, 76.0), (28.0, 82.0), (40.0, 66.0), (42.0, 76.0), (50.0, 64.0), (62.0, 80.0), (58.0, 64.0), (74.0, 76.0), (80.0, 76.0), (66.0, 64.0), (58.0, 60.0), (58.0, 70.0), (48.0, 60.0), (44.0, 72.0), (40.0, 64.0)], -- Varjostus
            color varjostus $ p [(12.0, 78.0), (20.0, 72.0), (14.0, 84.0), (14.0, 78.0), (12.0, 78.0)],
            color varjostus $ p [(16.0, 86.0), (20.0, 82.0), (20.0, 90.0), (28.0, 82.0), (32.0, 92.0), (36.0, 84.0), (38.0, 96.0), (42.0, 90.0), (46.0, 98.0), (48.0, 86.0), (54.0, 92.0), (52.0, 76.0), (66.0, 94.0), (68.0, 82.0), (72.0, 84.0), (76.0, 92.0), (70.0, 86.0), (68.0, 96.0), (72.0, 104.0), (58.0, 90.0), (60.0, 102.0), (54.0, 98.0), (56.0, 108.0), (48.0, 98.0), (48.0, 110.0), (42.0, 98.0), (36.0, 108.0), (36.0, 90.0), (30.0, 100.0), (28.0, 88.0), (18.0, 96.0), (18.0, 88.0), (14.0, 92.0), (12.0, 90.0), (16.0, 86.0)] -- Varjostu
        ]
    where
        juuri = makeColor 0.4 0.3 0.1 1
        havusto = makeColor 0.5 0.7 0.3 1
        varjostus = dark juuri
        p = piirräSprite


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
            Puu x -> piirräMaanpinnalle (x, korkeus' puu_mitat) puuSprite

