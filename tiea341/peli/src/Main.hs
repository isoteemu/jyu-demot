module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Data.List
import Assets
import Aritmetiikka

{-
    Harhauduin aikaslailla harjoitustyöstä.

    Pelimekaniikan muutoksia:
        - Intronäyttö
        - Maa on pyöreä, ei litteä.
        - Skybox
        - Ei pelasteta, vaan kaapataan

    Teknisiä muutoksia:
        - Kulma on kaikkialla radiaaneja.
        - kulma ja voima -vektorit fysikan laskemiseen
        - Satunnaisluvut ovat Doomista kopioitu lista lukuja. Tarjoaa riittävän satunnaisuuden, mutta ei tarvitse
          huolehtia satunnaislukujen ikävästä taipumuksesta olla satunnaisia. Doctor Whon sanoin:
            "Satunnaisuuden suuri vika on ettei se suosi hyviä vaihtoehtoja yli huonojen."


-}

data Pelitila = GameOver Ufolifter | GameOn Ufolifter | Intro Ufolifter deriving Show

-- | Maanpinnan taso vakiona elämän helpottamiseksi
maanpinnan_taso :: Num p => p
maanpinnan_taso = 5000

data Ufolifter = Peli {
    taso_aika        :: Float,
    taso_pisteet     :: Natural,

    taso_ufo         :: Ufo,

    taso_navetat    :: [Navetta],
    taso_lehmät     :: [Lehmä],

    ikkunan_leveys  :: Float,
    ikkunan_korkeus :: Float

} deriving(Show)

data Ufo = Ufo {
    ufo_sijainti    :: Point,
    ufo_teho        :: Vector,
    ufo_liike       :: Vector
} deriving (Show)

data Taso = Taso {}

data Navetta = Navetta {
    navetta_sijainti    :: Float
} deriving (Show)

data Lehmä = Lehmä {
    lehmä_sijainti  :: Point,
    lehmä_liike     :: Vector,

    lehmä_ai        :: LehmänAI
} deriving (Show)

data LehmänAI = AktiiviLehmä | PassiiviLehmä deriving Show

(>>==) ::  Pelitila -> (Ufolifter -> Pelitila) -> Pelitila
a >>== f1 = case a of
  GameOn peli -> f1 peli
  _ -> a


-- | Komenna ufoa.
-- | Vastaa esimerkin `kopterille`
ufolle :: (Ufo -> Ufo) -> Ufolifter -> Ufolifter 
ufolle f peli = peli {taso_ufo=f (taso_ufo peli)}


muutaTehoa :: Float -> Ufo -> Ufo
muutaTehoa teho_muutos hahmo = hahmo { 
                                    ufo_teho = (ufo_teho hahmo) #+ (0,teho_muutos)
                                }


kallista :: Float -> Ufo -> Ufo
kallista kulma_muutos hahmo = hahmo {
                                    ufo_teho = (ufo_teho hahmo) #+ (kulma_muutos,0)
                                }


-- | Palauta ufon liikevektori
ufonLiike :: Ufolifter -> Vector
ufonLiike peli = ufo_liike (taso_ufo peli)


-- | Palauta ufon tehovektori
ufonTeho :: Ufolifter -> Vector
ufonTeho peli = ufo_teho (taso_ufo peli)


-- | Palauta ufon sijainti
ufonSijainti :: Ufolifter -> Point
ufonSijainti peli = ufo_sijainti (taso_ufo peli)


-- | Palauta ufon etäisyys maanpinnasta
etäisyysMaasta :: Ufolifter -> Float
etäisyysMaasta peli = (pisteEtäisyys (0,0) (ufonSijainti peli)) - maanpinnan_taso


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

-- törmääköTaloon :: Point -> Float -> [Navetta] -> Bool
-- törmääköTaloon paikka kulma talot = all törmääköYhteen talot
--     where
--      törmääköYhteen talo 
--         = let 
--             ((ala1,ala2),(ylä1,ylä2)) = kopteriTörmäysviivat paikka kulma
--             (va,oy)   = nurkkaPisteet talo 
--           in segClearsBox ala1 ala2 va oy && segClearsBox ylä1 ylä2 va oy

-- nurkkaPisteet :: Navetta -> (Point,Point)
-- nurkkaPisteet talo = 
--     let
--         vasenAla = (navetta_sijainti talo - (navetta_leveys talo / 2) , 0)
--         oikeaYlä = (navetta_sijainti talo + (navetta_leveys talo / 2)      , navetta_korkeus talo) 
--     in (vasenAla,oikeaYlä)


reagoi :: Event -> Pelitila -> Pelitila
-- | Hanskaa ikkunan koon muutokset
reagoi (EventResize (w,h)) tila =
    let
        leveys = fromIntegral w :: Float
        korkeus = fromIntegral h :: Float
    in trace "Ikkunan koon muutos" $ case tila of
        GameOn peli -> GameOn peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        GameOver peli -> GameOver peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        Intro peli -> Intro peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}

-- | Käynnistä peli millä tahansa näppäimellä introissa
reagoi tapahtuma (Intro peli) = case tapahtuma of
    EventKey _ Down _ _                     -> trace "Käynnistä peli" GameOn peli
    EventKey (SpecialKey KeySpace) Down _ _ -> GameOn peli
    _                                       -> trace ("Tuntematon tapahtuma " <> show tapahtuma) Intro peli

-- | Reagoi pelin hallintapyyntöihin
reagoi tapahtuma (GameOn peli) = GameOn (
    case tapahtuma of
        EventKey (Char 'w') Down _ _ -> ufolle (muutaTehoa 4) peli
        EventKey (Char 's') Down _ _ -> ufolle (muutaTehoa (-4)) peli
        EventKey (Char 'a') Down _ _ -> ufolle (kallista 0.1) peli
        EventKey (Char 'd') Down _ _ -> ufolle (kallista (-0.1)) peli
        _                            -> peli
    )

reagoi _ tila = tila


-- | Varmista ufon etäisyys planeetasta
varmistaEtäisyys :: Ufolifter -> Pelitila
varmistaEtäisyys peli
        -- Lentääkö ulos kentän rajoista
        | lentokorkeus > maanpinnan_taso * 0.2 = trace ("Out of bounds") $ GameOver peli

        -- Saavuutaa maanpinnan
        | lentokorkeus <= 0 =
                            if (abs laskeutumis_kulma) <= 0.2 && momentti < 3 then
                                -- Ufo saavutti turvallisesti maan tason.
                                GameOn (ufolle laskeudu peli)
                            else
                                trace ("Törmäys maahan") $ GameOver peli

        -- Lentely jatkuu normaalisti
        | otherwise = GameOn peli 
    where
        (_, momentti)   = ufonLiike peli
        (kulma, teho)   = ufonTeho peli
        maan_kulma      = pisteKulma (0,0) (ufonSijainti peli)

        -- Maan kulman ja ufon kulman tulisi olla vastakkaiset
        laskeutumis_kulma = maan_kulma - kulma
        lentokorkeus = etäisyysMaasta peli

        laskeudu :: Ufo -> Ufo
        laskeudu ufo = ufo {
                -- Ufo maan suuntaiseksi
                ufo_teho=(maan_kulma, teho), 
                -- HACK miten saada ufo pysäytetyksi, muttei liimattua maahan
                ufo_liike=(kulma, max (teho-3) 0)
            }
                


päivitäPelitila :: Float -> Pelitila -> Pelitila
-- | Introssa ei päivitetä kuin kumulatiivinen aika
päivitäPelitila aika (Intro peli) = Intro peli{taso_aika=taso_aika peli + aika}


-- | Päivitä pelitilanne, ja varmista ettei ajauduta kuolettavaan tilanteeseen.
päivitäPelitila aika (GameOn peli) = tila
    where
        frame = päivitäPeli aika peli
        -- Varmista ettei olla liian kaukana planeetasta
        tila = GameOn (frame) >>== varmistaEtäisyys
päivitäPelitila aika (GameOver peli) = GameOver (päivitäPeli 0 peli)


-- | Päivitä pelitilanne.
päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika peli = 
        let
            edellinen_aika  = taso_aika peli
            (x, y)          = ufonSijainti peli
            teho            = ufonTeho peli
            liike           = ufonLiike peli

            sijainti = (x,y) #+ (pisteVektorista liike)

            -- Lisää ehdollisesti painovoima, jos ilmassa.
            lisää_painovoima momentti
                    | (etäisyysMaasta peli) > toleranssi = liikeVektori momentti painovoima aika
                    | otherwise = liike
                where
                    toleranssi = 1
                    painovoima = ((vektorienKulma (0,0) ( ufon_vektori #+ (pi, 0))), 4)

            ufon_vektori = vektoriPisteestä (x,y)
            (uusi_kulma, uusi_teho) = lisää_painovoima (liikeVektori liike teho aika)

            -- Lisää dragia
            uusi_liike = (uusi_kulma, uusi_teho*(1-(0.95*aika)))
        in
            peli {
                taso_aika=aika + edellinen_aika, 
                taso_ufo=(taso_ufo peli){ufo_sijainti=sijainti, ufo_liike=uusi_liike}
            }

piirräPeli :: Pelitila -> Picture
piirräPeli (Intro peli) = intro peli
piirräPeli (GameOn peli) = 
        let
            aika               = taso_aika peli
            (x, y)             = ufonSijainti peli
            (p_kulma, p_voima) = ufonTeho peli
            (m_kulma, m_voima) = ufonLiike peli

            ufo = translate x y $ rotate (-(radToDeg p_kulma) + 90) $ lautanen aika

            -- abs on jäänne debukkauksesta. Todellisuudessa aluksen ei koskaan tulisi työntyä maan sisään
            kameran_zoom = min 2 $ sqrt (ikkunan_korkeus peli / (abs (etäisyysMaasta peli) + 40) * 0.8)
            -- kameran_zoom = 0.1

            -- Varjo joka seuraa ufoa maassa.
            -- TODO: KORJAA
            varjo = translate x (maanpinnan_taso - 160 + (120*kerroin)) $ scale (1.0*kerroin) (0.4*kerroin) $ varjo_grafiikka
                where
                    wh = ikkunan_korkeus peli * 2
                    kerroin = max 0 ((wh - y )/ wh)
                    varjo_grafiikka = color (makeColor 0 0 0 kerroin) (circleSolid 100)

            ilmakehä = (color (light blue) $ circle (maanpinnan_taso * 1.2))

            skene = color (makeColor 0 1 0 0.4) (translate 0 0 (circleSolid maanpinnan_taso))
                <> varjo
                <> ufo
                <> ilmakehä
                -- <> Pictures (map (piirräNavetta peli) (taso_navetat peli))
                -- <> apuviivaAla <> apuviivaYlä
                -- <> törmäys

            debug = scale 0.2 0.2 (
                text (
                    "Teho: " <> (show $ round p_voima) 
                        <> " Kulma: " <> (show $ (round (p_kulma * 1e3)))
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
                <> (scale kameran_zoom kameran_zoom (scale 0.5 0.5 (translate (-x) (-y) (translate 0 (80) skene))))
                <> scanLines peli
                <> translate (-(ikkunan_leveys peli)/2) (-(ikkunan_korkeus peli)/2) (color white debug)


piirräPeli (GameOver _) = text "Game Over"


-- | Luo skybox, joka muuttaa intensiteettiä ufon korkeuden mukaan
tähtiTaivas :: Ufolifter -> Picture
tähtiTaivas peli =
    let
        tähtiä          = 120

        -- Tummenna taivasta mitä korkeammalla lennetään.
        w                   = ikkunan_leveys peli
        h                   = ikkunan_korkeus peli
        etäisyys_maasta     = abs (etäisyysMaasta peli)
        ilmakehän_korkeus   = maanpinnan_taso * 0.2

        intensiteetti       = min 1 $ max 0 (1 - (etäisyys_maasta / ilmakehän_korkeus))
        tausta_väri         = makeColor 0 0 intensiteetti 1
        tausta              = color tausta_väri $ rectangleSolid w h

        lisää_tähti n = translate x y $ color väri $ circleSolid koko
            where
                x = (sattuma !! n * ikkunan_leveys peli) - ikkunan_leveys peli / 2
                y = (sattuma !! (n+1) * ikkunan_korkeus peli) - ikkunan_korkeus peli / 2

                r = sattuma !! (n+0)
                g = sattuma !! (n+1)
                b = sattuma !! (n+2)
                väri = makeColor r g b (1-intensiteetti-0.2)
                koko = r + g + b

        in
            tausta
            <> Pictures (map lisää_tähti (take tähtiä [0..] ))


-- | Piirrä maanpinnalle. X pisteen tulee olla 0 - 1
piirräMaanpinnalle :: Point -> Picture -> Picture
piirräMaanpinnalle (x, y) = rotate ((pi * 2) * x) . translate (0) (maanpinnan_taso + y)


piirräNavetta ::  Navetta -> Picture
piirräNavetta navetta = piirräMaanpinnalle ((navetta_sijainti navetta), (navetan_korkeus)) navettaSprite

-- | Piirretään puita.
piirräPuita peli puut = Pictures puu_spritet
    where
        maanpinta = (maanpinnan_taso + 80)
        piirrä_puu  = undefined
        puu_spritet = undefined


-- | Tee ikkunan korkuinen ja levyinen elementti, joka peittää osan kuvasta viivoilla.
scanLines :: Ufolifter -> Picture
scanLines peli =
    let
        joka_nth    = 2
        x           = ikkunan_leveys peli
        korkeus     = ikkunan_korkeus peli

        väri        = makeColor 0 0.1 0 0.6
        rivejä      = floor (korkeus * 2 / joka_nth)
        viiva rivi  = color väri (line [(-(x/2), y), (x/2, y)])
                        where y = -korkeus + (rivi * joka_nth)
    in
        Pictures (map viiva (take rivejä [0..]))


-- | Intronäyttö
intro :: Ufolifter -> Picture
intro peli =
    let
        aika = taso_aika peli
        viivoja = 21 :: Int -- Poukkoilevien viivojen määrä
        viiva = (round aika)
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
                r = sattuma !! (n `mod` viivoja + 0)
                g = sattuma !! (n `mod` viivoja + 1)
                b = sattuma !! (n `mod` viivoja + 2)
                a =  max 0 (sin(aika-n_f) :: Float)
                väri = makeColor r g b a
                pituus = sqrt (w * h) * ((r + b + g) / 3)
        ufo_x = w * sin(aika*1.2) * 0.1
        ufo_y = h * sin(aika*0.8) * 0.1
    in
        Pictures (map f (map kasvata (take viivoja [1..])))
        <> (translate ufo_x ufo_y $ scale 0.5 0.5 $ lautanen aika)
        <> (translate (-w/2) (h/2) introTitleSprite)
        <> (translate (-60) (-(h/2)+20) $ scale 0.15 0.15  $ color spacen_väri $ text "[ SPACE ]")
        <> scanLines peli


-- | Palauttaa listan satunnaisia lukuja 
sattuma :: Fractional a => [a]
sattuma = map (/255) satunnaisTaulukko


luoNavettoja :: Integer -> [Navetta]
luoNavettoja määrä = [Navetta 0]


alustaTaso :: Ufolifter
alustaTaso = Peli
            {
                taso_aika       = 0,
                taso_pisteet    = 0,

                taso_ufo        = alustaUfo,

                taso_navetat    = luoNavettoja 1,
                taso_lehmät     = [],

                -- Ikkunan leveys ja korkeus asetetaan tapahtumassa.
                ikkunan_leveys  = -1, 
                ikkunan_korkeus = -1
            }

alustaUfo :: Ufo
alustaUfo = Ufo
            {
                -- Voima tehossa ja liikkeessä perustuu ufon sulavaan laskeutumiseen
                ufo_sijainti = (0,5600),
                ufo_teho     = (pi/2, 4),
                ufo_liike    = (pi*1.5, 18)
            }

alustaPeli :: Pelitila
alustaPeli = GameOn alustaTaso

main :: IO ()
-- main = animate 
--             (InWindow "Choplifter" (600,400) (200,200))
--             (light blue)
--             (\x -> lehmäSprite)
         
main = do
    play
        (InWindow "Ufolifter" (720,480) (300,5200))
        black
        30
        (Intro alustaTaso)
        piirräPeli
        reagoi
        päivitäPelitila
