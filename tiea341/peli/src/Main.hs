module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Graphics.Gloss.Data.ViewPort

import Data.Fixed
import Assets

import Data.Tuple.Curry

ikkunan_leveys :: Num a => a
ikkunan_leveys = 600
ikkunan_korkeus :: Num a => a
ikkunan_korkeus = 400

data Pelitila = GameOver Ufolifter | GameOn Ufolifter

data Ufolifter = Peli {
    taso_aika        :: Float,

    ufo_sijainti    :: (Float,Float),
    ufo_teho        :: (Float,Float),
    ufo_liike       :: (Float,Float),

    taso_navetat    :: [Navetta],
    taso_lehmä      :: [Lehmä],

    view_port       :: ViewPort
    
}

data Navetta = Navetta {
    navetta_korkeus     :: Float,
    navetta_leveys      :: Float,
    navetta_sijainti    :: Float
}

data Lehmä = Lehmä {
    lehmä_sijainti :: (Float,Float),
    lehmä_liike    :: (Float,Float)
}

muutaTehoa :: Float -> Ufolifter -> Ufolifter
muutaTehoa teho_muutos peli = peli { ufo_teho = (ufo_teho peli) #+ (0,teho_muutos)}

kallista :: Float -> Ufolifter -> Ufolifter
kallista kulma_muutos peli = peli { ufo_teho = (ufo_teho peli) #+ (kulma_muutos,0)}

-- | Laske XY koordinaatit vektorille
sijaintiKulmastaJaVoimasta :: Vector -> Point
sijaintiKulmastaJaVoimasta (kulma, voima) = (x, y)
    where
        x = voima * cos( kulma )
        y = voima * sin( kulma )


-- | Laske kahden vektorin yhteenlaskettu pituus.
vektoriImpulssi :: Vector -> Vector -> Float
vektoriImpulssi a b = sqrt (x*x + y*y)
    where
        (a_x, a_y) = sijaintiKulmastaJaVoimasta a 
        (b_x, b_y) = sijaintiKulmastaJaVoimasta b
        x = a_x + b_x
        y = a_y + b_y


-- | Laske kahden vektorin etäisyyden kulma
vektorienKulma :: Vector -> Vector -> Float
vektorienKulma a b = kulma
    where
        (a_x, a_y) = sijaintiKulmastaJaVoimasta a 
        (b_x, b_y) = sijaintiKulmastaJaVoimasta b

        x = a_x + b_x
        y = a_y + b_y

        kulma = (atan2 y x)
        -- kulma' = (atan2 y x)

        -- kulma
        --     | kulma' < 0 = kulma' + pi*2
        --     | otherwise = kulma'


voimavektori :: Float -> Float -> (Float, Float)
voimavektori teho kulma = rotateV (- degToRad kulma) (0, teho)


-- | Lisää tehoa liike-vektoriin aikakertoimen verran.
liikeVektori :: Vector -> Vector -> Float -> Vector
liikeVektori liike (p_kulma, p_teho) aika = (
        kulma,
        etäisyys
    )
    where
        skaalattu_teho = (p_kulma, (p_teho * aika))

        kulma = vektorienKulma liike skaalattu_teho
        etäisyys = vektoriImpulssi liike skaalattu_teho



-- | Apufunktio vektorien yhteenlaskuun
(#+) :: Vector -> Vector -> Vector
(a,b) #+ (x,y) = (a+x, b+y)


kopteriTörmäysviivat :: Point -> Float -> ((Point,Point) , (Point,Point))
kopteriTörmäysviivat paikka kulma = 
    let
     vasen = -170
     oikea = 100 
     kääntö = rotateV (kulma - pi/2)
    in (  (kääntö (vasen,0) #+ paikka
          ,kääntö (oikea,0) #+ paikka)
          ,
          (kääntö (vasen,120) #+ paikka
          ,kääntö (oikea,120) #+ paikka)
 
       )

törmääköTaloon :: Point -> Float -> [Navetta] -> Bool
törmääköTaloon paikka kulma talot = all törmääköYhteen talot
    where
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = kopteriTörmäysviivat paikka kulma
            (va,oy)   = nurkkaPisteet talo 
          in segClearsBox ala1 ala2 va oy && segClearsBox ylä1 ylä2 va oy

nurkkaPisteet :: Navetta -> (Point,Point)
nurkkaPisteet talo = 
    let
        vasenAla = (navetta_sijainti talo - (navetta_leveys talo / 2) , 0)
        oikeaYlä = (navetta_sijainti talo + (navetta_leveys talo / 2)      , navetta_korkeus talo) 
    in (vasenAla,oikeaYlä)


reagoi :: Event -> Pelitila -> Pelitila
reagoi _ (GameOver peli) = GameOver peli
reagoi tapahtuma (GameOn peli) = GameOn (
    case tapahtuma of
        EventKey (Char 'w') Down _ _ -> muutaTehoa 4 peli
        EventKey (Char 's') Down _ _ -> muutaTehoa (-4) peli 
        EventKey (Char 'a') Down _ _ -> kallista 0.1 peli 
        EventKey (Char 'd') Down _ _ -> kallista (-0.1) peli 
        _ -> peli
    )


päivitäPelitila :: Float -> Pelitila -> Pelitila
päivitäPelitila aika (GameOn peli) = GameOn (päivitäPeli aika peli)
päivitäPelitila aika (GameOver peli) = GameOver (päivitäPeli 0 peli)


päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika peli = 
        let
            edellinen_aika  = taso_aika peli
            (x, y)          = ufo_sijainti peli
            teho            = ufo_teho peli
            liike           = ufo_liike peli

            painovoima = (pi*1.5, 3)
            (uusi_kulma, uusi_teho) = liikeVektori (liikeVektori liike teho aika) painovoima aika

            -- Lisää dragia
            uusi_liike = (uusi_kulma, uusi_teho*(1-(0.95*aika)))

            (vx, vy) =  sijaintiKulmastaJaVoimasta uusi_liike

            dx = x + vx 
            dy = y + vy

            sijainti = (dx, max (-40) dy)

        in  peli {taso_aika=aika + edellinen_aika, ufo_sijainti=sijainti, ufo_liike=uusi_liike}


piirräPeli :: Pelitila -> Picture
piirräPeli (GameOn peli) = 
        let
            aika    = taso_aika peli
            (x, y)  = ufo_sijainti peli
            (p_kulma, p_voima) = ufo_teho peli
            (m_kulma, m_voima) = ufo_liike peli

            navetat = taso_navetat peli


            ((va,oa), (va1,oa1)) = kopteriTörmäysviivat (x, y) kulma
            apuviivaAla = color red (line [va,oa])
            apuviivaYlä = color red (line [va1,oa1])

            eiTörmää = törmääköTaloon (x,y) kulma (taso_navetat peli)
            törmäys
                    | eiTörmää == False  = scale 0.4 0.4 $ color red $  text "CRASH"
                    | otherwise = blank

            (kulma, teho) = ufo_teho peli
            ufo = translate x y $ rotate (-(radToDeg p_kulma) + 90) $ lautanen aika

            -- Varjo joka seuraa ufoa maassa.
            varjo = translate x (-160 + (120*kerroin)) $ scale (1.0*kerroin) (0.4*kerroin) $ varjo_grafiikka
                where
                    wh = ikkunan_korkeus * 2
                    kerroin = max 0 ((wh - y )/ wh)
                    varjo_grafiikka = color (makeColor 0 0 0 kerroin) (circleSolid 100)

            skene = color green (translate 0 (-500) (rectangleSolid 5000 1000))
                <> varjo
                <> ufo
                -- <> Pictures (map piirräNavetta navetat)
                -- <> apuviivaAla <> apuviivaYlä
                -- <> törmäys

            debug = scale 0.2 0.2 (
                text (
                    "Teho: " <> (show $ round p_voima) <> " Kulma: " <> (show $ (round (p_kulma * 1e3))) <> "\n"
                    )
                )
                <> translate (ikkunan_leveys - 50) (50) (
                    color white (
                        circle 20
                        <> line [(0,-22),(0,22)] <> line [(-22,0),(22,0)]
                        )
                    <> rotate (-radToDeg m_kulma) (scale (m_voima/10) 1 (color red (translate (10) 0 $ rectangleSolid 20 3)))
                )

        in
            tähtiTaivas peli
                <> scale 0.5 0.5 (translate 0 (-180) skene)
                -- <> scanLines
                <> translate (-ikkunan_leveys/2) (-ikkunan_korkeus/2) (color white debug)

piirräPeli (GameOver _) = text "Game Over"

lautanen :: Float -> Picture
lautanen aika = color väri (translate 0 korkeus keho)
    where
        -- Ufon paramtrejä
        väri = greyN 0.9
        leveys = 100
        korkeus = 40
        nopeus = 3

        aika' = aika * 0.8

        lisääPallura offset
            | luku <= ositus || luku >= ositus * 3 = pallura
            | otherwise = blank
            where
                ositus = pi*2/4
                luku = (aika' * nopeus + offset) `mod'` (pi*2)
                skaala = cos(luku)
                sijainti = sin(luku) * (-1)
                pallura = translate (sijainti*leveys-sijainti) 0 (scale skaala 1 $ color blue (circleSolid 10))

        runko = (scale 1.0 0.4 (circleSolid leveys))
            <> translate 0 (korkeus * 0.85) (scale 1.0 0.6 (circleSolid korkeus)) -- Ohjaamo
            <> translate 0 (korkeus * (-0.9)) (scale 1.0 0.2 ( -- Imutusreikä
                circleSolid korkeus
                <> (color (light yellow) (circleSolid (korkeus*0.9)))
            ))
        -- Lisää pyörivät pallurat
        keho = foldr (\n runko' -> runko' <> lisääPallura (pi*2/6*n)) runko [1..6]


tähtiTaivas :: Ufolifter -> Picture
tähtiTaivas peli = let
        -- Tummenna taivasta mitä korkeammalla lennetään.
        w = ikkunan_leveys
        h = ikkunan_korkeus
        y = snd $ ufo_sijainti peli
        intensiteetti = min 1 $ max 0 ( 1 - (y / (h * 2)))
        tausta_väri = makeColor 0 0 intensiteetti 1
        tausta = color tausta_väri $ rectangleSolid w h

        -- TODO: Lisää tähtiä

        in
            tausta

piirräNavetta :: Navetta -> Picture
piirräNavetta navetta = elementti
    where
        w = navetta_leveys navetta
        h = navetta_korkeus navetta
        sprite =  rectangleSolid w h
        elementti = translate (navetta_sijainti navetta) (navetta_korkeus navetta * 0.2) sprite


{-
    Tee ikkunan korkuinen ja levyinen elementti, joka peittää osan kuvasta viivoilla.
-}
scanLines :: Picture
scanLines = let
    joka_nth = 2
    väri = makeColor 0 0 0 0.6
    x = ikkunan_leveys
    korkeus = ikkunan_korkeus
    rivejä = floor (korkeus * 2 / joka_nth)
    viiva rivi = color väri (line [(-(x/2), y), (x/2, y)])
        where y = -korkeus + (rivi * joka_nth)
    in foldr (\n buffer -> buffer <> viiva n) blank (take rivejä [0..])

-- tähti ::[(Float, Float)]
-- tähti = let
--         astetta = pi * 2 / 5 * 2
--         pisteytä = \seg pisteet -> let
--                 n = fromIntegral seg :: Float
--                 pos = (sin (astetta * n), cos (astetta * n))
--             in  pos:pisteet
--     in
--         foldr pisteytä [] (take (5+1) [1..])

main :: IO ()
main = do

    play
        (InWindow "Ufolifter" (ikkunan_leveys,ikkunan_korkeus) (300,200))
        blue
        30
        (GameOn
            (Peli 0
                (0,100) (pi/2,0) (0,0)
                [Navetta 100 200 200]
                []
            )
        )
        piirräPeli
        reagoi
        päivitäPelitila
