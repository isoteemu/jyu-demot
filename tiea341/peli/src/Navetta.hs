module Navetta where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Line
import Peli
import Aritmetiikka
import Assets
import Graphics.Gloss

data Navetta = Navetta {
    navetta_sijainti    :: Float
} deriving (Show)

navetta_mitat :: (Point, Point)
navetta_mitat = ((0, 0), (215, 120))


navettaSprite :: Picture
navettaSprite = Pictures [
                            color seinä $ p [(0.0, 50.0), (50.0, 0.0), (100.0, 50.0), (100.0, 120.0), (0.0, 120.0), (0.0, 50.0)], -- Etufakaadi
                            color karmi $ p [(20.0, 120.0), (18.0, 120.0), (18.0, 58.0), (82.0, 58.0), (82.0, 120.0), (80.0, 120.0), (80.0, 60.0), (20.0, 60.0), (20.0, 120.0)], -- Karmi
                            color black $ p [(20.0, 120.0), (20.0, 60.0), (80.0, 60.0), (80.0, 120.0), (20.0, 120.0)], -- Oviaukko
                            color (dark katto) $ p [(48.0, 0.0), (-2.0, 50.0), (0.0, 50.0), (50.0, 0.0), (48.0, 0.0)], -- katto-takamus
                            color (dark seinä) $ p [(100.0, 50.0), (100.0, 120.0), (210.0, 120.0), (210.0, 50.0), (100.0, 50.0)], -- sivuseinä
                            color katto $ p [(48.0, 0.0), (162.0, 0.0), (212.0, 50.0), (98.0, 50.0), (48.0, 0.0)], -- katto
                            color (greyN 0.8) $ p [(24.0, 60.0), (24.0, 120.0), (20.0, 120.0), (20.0, 60.0), (24.0, 60.0)],

                            -- Ikkuna
                            color karmi $ p [(38.0, 26.0), (60.0, 26.0), (60.0, 46.0), (38.0, 46.0), (38.0, 26.0)],
                            color black $ p [(40.0, 44.0), (58.0, 44.0), (58.0, 28.0), (40.0, 28.0), (40.0, 44.0)],
                            color (greyN 0.8) $ p [(40.0, 28.0), (40.0, 44.0), (42.0, 44.0), (42.0, 28.0), (40.0, 28.0)]
                         ]
                where
                    seinä = red
                    katto = greyN 0.7
                    karmi = white
                    p = piirräSprite


-- | Luo satunnaisia navettoja tasolle
-- TODO: Puut ja navetat tulee samoille kohdin
luoNavettoja :: Int -> [Navetta]
luoNavettoja määrä = [porotokka] ++ map (\i -> Navetta (i * maanpinnan_taso * 2)) (take (määrä - 1) sattuma)
    where
        -- Yksi navetta on aina pohjoisnavalla
        porotokka = Navetta (maanpinnan_taso * (-0.5))

navetanTörmäysviiva :: Navetta -> (Point, Point)
navetanTörmäysviiva navetta = 
                            let
                                sijainti = (navetta_sijainti navetta, 0)
                                a = sijainti #+ fst navetta_mitat
                                b = sijainti #+ snd navetta_mitat
                             in
                                pisteet $ koordinaatiKaarella [a,b]


törmääköNavettaa :: ((Point,Point),(Point,Point)) -> [Navetta] -> Bool
törmääköNavettaa törmäysviivat talot = 
        not $ and $ map (törmääköYhteen) talot
    where
        törmääköYhteen talo =
            let
                ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
                (va,oy)                   = navetanTörmäysviiva talo
            in (segClearsBox ala1 ala2 va oy) && (segClearsBox ylä1 ylä2 va oy)


piirräNavetta ::  Navetta -> Picture
piirräNavetta navetta = piirräMaanpinnalle ((navetta_sijainti navetta), navetan_korkeus) navettaSprite
    where
        ((_,a),(_,b)) = navetta_mitat
        navetan_korkeus = a + b

