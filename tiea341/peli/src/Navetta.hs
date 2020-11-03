module Navetta where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Line
import Peli
import Aritmetiikka
import Assets

data Navetta = Navetta {
    navetta_sijainti    :: Float
} deriving (Show)

navetta_mitat :: (Point, Point)
navetta_mitat = ((0, 0), (215, 120))

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

