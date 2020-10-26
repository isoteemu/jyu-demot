module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Data.Fixed
import Data.List
import Assets

{-
    Pelimekaniikan muutoksia:
        - Intronäyttö
        - Maa on pyöreä, ei litteä.

    Teknisiä muutoksia:
        - Kulma on kaikkialla radiaaneja.
        - Liikkeet ja voimat ei pelissä XY pisteitä (`Point`), vaan liikevektroeita (Angle, Force)
-}

data Pelitila = GameOver Ufolifter | GameOn Ufolifter | Intro Ufolifter deriving Show

data Ufolifter = Peli {
    taso_aika        :: Float,

    ufo_sijainti    :: Point,
    ufo_teho        :: Vector,
    ufo_liike       :: Vector,

    ufo_pisteet     :: Natural,

    taso_navetat    :: [Navetta],
    taso_lehmä      :: [Lehmä],

    taso_koko       :: Float,

    ikkunan_leveys  :: Float,
    ikkunan_korkeus :: Float

} deriving Show

data Navetta = Navetta {
    navetta_korkeus     :: Float,
    navetta_leveys      :: Float,
    navetta_sijainti    :: Float
} deriving (Show)

data Lehmä = Lehmä {
    lehmä_sijainti  :: (Float,Float),
    lehmä_liike     :: (Float,Float),

    lehmä_ai        :: LehmänAI
} deriving (Show)

data LehmänAI = AktiiviLehmä | PassiiviLehmä deriving Show

(>>==) ::  Pelitila -> (Ufolifter -> Pelitila) -> Pelitila
a >>== f1 = case a of
  GameOn peli -> f1 peli
  _ -> a


muutaTehoa :: Float -> Ufolifter -> Ufolifter
muutaTehoa teho_muutos peli = peli { ufo_teho = (ufo_teho peli) #+ (0,teho_muutos)}


kallista :: Float -> Ufolifter -> Ufolifter
kallista kulma_muutos peli = peli { ufo_teho = (ufo_teho peli) #+ (kulma_muutos,0)}


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


vektoriEtäisyys :: Vector -> Vector -> Float
vektoriEtäisyys a b = sqrt (x*x + y*y)
    where
        (a_x, a_y) = pisteVektorista a 
        (b_x, b_y) = pisteVektorista b
        x = a_x - b_x
        y = a_y - b_y


-- | Laske kahden vektorin etäisyyden kulma
vektorienKulma :: Vector -> Vector -> Float
vektorienKulma a b = kulma
    where
        (a_x, a_y) = pisteVektorista a 
        (b_x, b_y) = pisteVektorista b

        x = a_x + b_x
        y = a_y + b_y

        kulma = (atan2 y x)
        -- kulma' = (atan2 y x)

        -- kulma
        --     | kulma' < 0 = kulma' + pi*2
        --     | otherwise = kulma'


voimavektori :: Float    -> Float -> (Float, Float)
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
        etäisyys = vektoriLäheisyys liike skaalattu_teho



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
-- | Hanskaa ikkunan koon muutokset
reagoi (EventResize (w,h)) tila = let
        leveys = fromIntegral w ::Float
        korkeus = fromIntegral h ::Float
    in case tila of
        GameOn peli -> GameOn peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        GameOver peli -> GameOver peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        Intro peli -> Intro peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}

-- | Käynnistä peli millä tahansa näppäimellä introissa
reagoi tapahtuma (Intro peli) = case tapahtuma of
    EventKey _ Down _ _ -> trace "Käynnistä peli" GameOn peli
    EventKey (SpecialKey KeySpace) Down _ _ -> GameOn peli
    _ -> trace ("Tuntematon tapahtuma" <> show tapahtuma) Intro peli

-- | Reagoi pelin hallintapyyntöihin
reagoi tapahtuma (GameOn peli) = GameOn (
    case tapahtuma of
        EventKey (Char 'w') Down _ _ -> muutaTehoa 4 peli
        EventKey (Char 's') Down _ _ -> muutaTehoa (-4) peli 
        EventKey (Char 'a') Down _ _ -> kallista 0.1 peli 
        EventKey (Char 'd') Down _ _ -> kallista (-0.1) peli 
        _ -> peli
    )

reagoi tapahtuma tila = trace ("Tapathuma tuntemattomassa tilassa" <> show tila) tila


-- | Varmista ufon etäisyys planeetasta
varmistaEtäisyys :: Ufolifter -> Pelitila
varmistaEtäisyys peli
    | lentokorkeus > taso_koko peli + (ikkunan_korkeus peli*2) = GameOver peli
    | otherwise = GameOn peli
    where
        lentokorkeus = vektoriEtäisyys (0,0) (vektoriPisteestä (ufo_sijainti peli))


päivitäPelitila :: Float -> Pelitila -> Pelitila
päivitäPelitila aika (Intro peli) =
        Intro peli{taso_aika=aika_summa}
    where
        aika_summa = taso_aika peli + aika

päivitäPelitila aika (GameOn peli) = tila
    where
        frame = päivitäPeli aika peli
        -- Varmista ettei olla liian kaukana planeetasta
        tila = GameOn (frame) >>== varmistaEtäisyys
päivitäPelitila aika (GameOver peli) = GameOver (päivitäPeli 0 peli)


päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika peli = 
        let
            edellinen_aika  = taso_aika peli
            (x, y)          = ufo_sijainti peli
            teho            = ufo_teho peli
            liike           = ufo_liike peli

            ufon_vektori = vektoriPisteestä (x,y)
            painovoima = ((vektorienKulma (0,0) ( ufon_vektori #+ (pi, 0))), 4)
            (uusi_kulma, uusi_teho) = liikeVektori (liikeVektori liike teho aika) painovoima aika

            -- Lisää dragia
            uusi_liike = (uusi_kulma, uusi_teho*(1-(0.95*aika)))

            (vx, vy) =  pisteVektorista uusi_liike

            dx = x + vx 
            dy = y + vy

            sijainti = (dx, max (-40) dy)

        in  peli {taso_aika=aika + edellinen_aika, ufo_sijainti=sijainti, ufo_liike=uusi_liike}

piirräPeli :: Pelitila -> Picture
piirräPeli (Intro peli) = intro peli
piirräPeli (GameOn peli) = 
        let
            aika    = taso_aika peli
            (x, y)  = ufo_sijainti peli
            (p_kulma, p_voima) = ufo_teho peli
            (m_kulma, m_voima) = ufo_liike peli

            säde = taso_koko peli

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
            varjo = translate x (säde - 160 + (120*kerroin)) $ scale (1.0*kerroin) (0.4*kerroin) $ varjo_grafiikka
                where
                    wh = ikkunan_korkeus peli * 2
                    kerroin = max 0 ((wh - y )/ wh)
                    varjo_grafiikka = color (makeColor 0 0 0 kerroin) (circleSolid 100)

            skene = color (makeColor 0 1 0 0.4) (translate 0 0 (circleSolid säde))
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
                <> translate (ikkunan_leveys peli - 50) (50) (
                    color white (
                        circle 20
                        <> line [(0,-22),(0,22)] <> line [(-22,0),(22,0)]
                        )
                    <> rotate (-radToDeg m_kulma) (scale (m_voima/10) 1 (color red (translate (10) 0 $ rectangleSolid 20 3)))
                )

        in
            tähtiTaivas peli
                <> (scale 0.5 0.5 (translate (-x) (-y) (translate 0 (80) skene)))
                <> scanLines peli
                <> translate (-(ikkunan_leveys peli)/2) (-(ikkunan_korkeus peli)/2) (color white debug)

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


-- | Luo skybox, joka muuttaa intensiteettiä ufon korkeuden mukaan
tähtiTaivas :: Ufolifter -> Picture
tähtiTaivas peli = let
        tähtiä = 120

        -- Tummenna taivasta mitä korkeammalla lennetään.
        w = ikkunan_leveys peli
        h = ikkunan_korkeus peli
        etäisyys = abs (vektoriEtäisyys (0,0) (ufo_sijainti peli)) - (taso_koko peli)

        intensiteetti = min 1 $ max 0 ( 1 - (etäisyys / (h * 2)))
        tausta_väri = makeColor 0 0 intensiteetti 1
        tausta = color tausta_väri $ rectangleSolid w h

        satunnaista = map (/255) satunnaisTaulukko

        lisää_tähti n = translate x y $ color väri $ circleSolid koko
            where
                x = (satunnaista !! n * ikkunan_leveys peli) - ikkunan_leveys peli / 2
                y = (satunnaista !! (n+1) * ikkunan_korkeus peli) - ikkunan_korkeus peli / 2

                r = satunnaista !! (n+0)
                g = satunnaista !! (n+1)
                b = satunnaista !! (n+2)
                väri = makeColor r g b (1-intensiteetti)
                koko = r + g + b

        in
            tausta
            <> Pictures (map lisää_tähti (take tähtiä [0..] ))

piirräNavetta :: Navetta -> Picture
piirräNavetta navetta = piirros
    where
        w = navetta_leveys navetta
        h = navetta_korkeus navetta
        piirros = translate (navetta_sijainti navetta) (navetta_korkeus navetta * 0.2) navettaSprite


-- | Tee ikkunan korkuinen ja levyinen elementti, joka peittää osan kuvasta viivoilla.
scanLines :: Ufolifter -> Picture
scanLines peli = let
    joka_nth = 2
    väri = makeColor 0 0 0 0.6
    x = ikkunan_leveys peli
    korkeus = ikkunan_korkeus peli
    rivejä = floor (korkeus * 2 / joka_nth)
    viiva rivi = color väri (line [(-(x/2), y), (x/2, y)])
        where y = -korkeus + (rivi * joka_nth)
    in Pictures (map viiva (take rivejä [0..]))


-- | Intronäyttö
intro :: Ufolifter -> Picture
intro peli =
    let
        aika = taso_aika peli
        viivoja = 21 :: Int -- Poukkoilevien viivojen määrä
        viiva = (round aika)
        color_values = map (/ 255) satunnaisTaulukko
        kasvata n = n + (fromIntegral viiva)

        w = ikkunan_leveys peli
        h = ikkunan_korkeus peli

        -- Välkytä tekstiä
        i = (abs $ sin aika*2)
        spacen_väri = (makeColor i i i 1)

        f :: Int -> Picture
        f n = color väri $ rotate kulma $ line [(0,0),(pituus,0)]
            where
                -- Laske "satunnaisille" viivoille arvot
                n_f = fromIntegral n :: Float

                -- Kulman arvot on kokeilemalla valittu
                kulma = radToDeg (n_f * (1.2 + (n_f*0.1)))
                r = color_values !! (n `mod` viivoja + 0)
                g = color_values !! (n `mod` viivoja + 1)
                b = color_values !! (n `mod` viivoja + 2)
                a =  max 0 (sin(aika-n_f) :: Float)
                väri = makeColor r g b a
                pituus = sqrt (w * h) * ((r + b + g) / 3)
        ufo_x = w * sin(aika*1.2) * 0.1
        ufo_y = h * sin(aika*0.8) * 0.1
    in
        Pictures (map f (map kasvata (take viivoja [1..])))
        <> (translate ufo_x ufo_y $ scale 0.5 0.5 $ lautanen aika)
        <> (translate (-w/2) (h/2) introUfoSprite)
        <> (translate (-60) (-(h/2)+20) $ scale 0.15 0.15  $ color spacen_väri $ text "[ SPACE ]")
        <> scanLines peli


alustaUfo :: Ufolifter
alustaUfo = Peli 0
                (0,5600) (pi/2,4) (pi*1.5,18) 0
                [Navetta 100 200 200]
                []
                5000 -- Maan koko

                600 400 -- Ikkunan koko.

alustaPeli :: Pelitila
alustaPeli = GameOn alustaUfo

main :: IO ()
-- main = animate 
--             (InWindow "Choplifter" (600,400) (200,200))
--             black
--             intro
         
main = do
    play
        (InWindow "Ufolifter" (600,400) (300,5200))
        black
        30
        (Intro alustaUfo)
        piirräPeli
        reagoi
        päivitäPelitila
