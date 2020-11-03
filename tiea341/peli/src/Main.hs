{-#OPTIONS_GHC -Wtype-defaults#-}

module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Geometry.Line

import Data.Fixed
import Data.List (partition,  (!!) )
import Peli
import Assets
import Aritmetiikka
import Ufo
import Navetta
import Lehmä
import Karikko

{-
    Pelimekaniikan muutoksia:
        - Intronäyttö
        - Maa on pyöreä, ei litteä.
        - Skybox
        - Ei pelasteta, vaan kaapataan
        - Ei ole erillisiä laskutelineitä. Törmäys on törmäys.

    Teknisiä muutoksia:
        - Kulma on kaikkialla radiaaneja.
        - kulma ja voima -vektorit fysikan laskemiseen
        - Satunnaisluvut ovat Doomista kopioitu lista lukuja. Tarjoaa riittävän satunnaisuuden, mutta ei tarvitse
          huolehtia satunnaislukujen ikävästä taipumuksesta olla satunnaisia. Doctor Whon sanoin:
            "Satunnaisuuden suuri puute on ettei se suosi hyviä vaihtoehtoja yli huonojen."

    Bugeja:
        - Painovoima ei välitä pelin pysäyttämisetä.
        - Tracktorbeam ei välitä osuuko se maahan vai ei.
-}

data Pelitila = GameOver Ufolifter Text | GameOn Ufolifter | Intro Ufolifter deriving Show

data Ufolifter = Peli {
    taso_aika        :: Float,
    taso_pisteet     :: Natural,

    taso_ufo         :: Ufo,

    taso_navetat    :: [Navetta],
    taso_lehmät     :: [Lehmä],
    taso_karikot    :: [Karikko],

    ikkunan_leveys  :: Float,
    ikkunan_korkeus :: Float

} deriving(Show)


data Taso = Taso {}

(>>==) ::  Pelitila -> (Ufolifter -> Pelitila) -> Pelitila
a >>== f1 = case a of
  GameOn peli -> f1 peli
  _ -> a


-- | Komenna ufoa.
-- | Vastaa esimerkin `kopterille`
ufolle :: (Ufo -> Ufo) -> Ufolifter -> Ufolifter 
ufolle f peli = peli {taso_ufo=f (taso_ufo peli)}


-- | Palauta ufon liikevektori
ufonLiike :: Ufolifter -> Vector
ufonLiike peli = ufo_liike (taso_ufo peli)

-- | Palauta ufon tehovektori
ufonTeho :: Ufolifter -> Vector
ufonTeho peli = ufo_teho (taso_ufo peli)

-- | Palauta ufon sijainti
ufonSijainti :: Ufolifter -> Point
ufonSijainti peli = ufo_sijainti (taso_ufo peli)


reagoi :: Event -> Pelitila -> Pelitila
-- | Hanskaa ikkunan koon muutokset
reagoi (EventResize (w,h)) tila =
    let
        leveys = fromIntegral w :: Float
        korkeus = fromIntegral h :: Float
    in trace "Ikkunan koon muutos" $ case tila of
        GameOn peli -> GameOn peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        GameOver peli syy -> GameOver peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus} syy
        Intro peli -> Intro peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}

-- | Käynnistä peli millä tahansa näppäimellä introissa
reagoi tapahtuma (Intro peli) = case tapahtuma of
    EventKey _ Down _ _                     -> GameOn peli
    EventKey (SpecialKey KeySpace) Down _ _ -> GameOn peli

    _                                       -> trace ("Tuntematon tapahtuma " <> show tapahtuma) Intro peli

-- | Reagoi pelin hallintapyyntöihin
reagoi tapahtuma (GameOn peli) = GameOn (
    case tapahtuma of
        EventKey (Char 'w') Down _ _ -> ufolle (muutaTehoa (-4)) peli
        EventKey (Char 's') Down _ _ -> ufolle (muutaTehoa 4) peli
        EventKey (Char 'a') Down _ _ -> ufolle (kallista 0.1) peli
        EventKey (Char 'd') Down _ _ -> ufolle (kallista (-0.1)) peli

        EventKey (SpecialKey KeySpace) Down _ _ -> ufolle (traktori Päällä) peli
        EventKey (SpecialKey KeySpace) Up _ _   -> ufolle (traktori Pois) peli

        _ -> peli
    )

reagoi _ tila = tila


-- | Varmista ufon etäisyys planeetasta
varmistaEtäisyys :: Ufolifter -> Pelitila
varmistaEtäisyys peli
        -- Lentääkö ulos kentän rajoista
        | lentokorkeus > maanpinnan_taso * 0.2 = GameOver peli "Poistuit ilmakehästä"

        -- Saavuutaa maanpinnan
        | lentokorkeus <= 0 =
                            if (abs laskeutumis_kulma) <= 0.2 && momentti < 3 then
                                -- Ufo saavutti turvallisesti maan tason.
                                GameOn (ufolle laskeudu peli)
                            else
                                GameOver peli "Törmäsit maahan"

        -- Lentely jatkuu normaalisti
        | otherwise = GameOn peli 
    where
        (_, momentti)   = ufonLiike peli
        (kulma, teho)   = ufonTeho peli
        maan_kulma      = pisteKulma (0,0) (ufonSijainti peli)

        laskeutumis_kulma = maan_kulma - kulma
        lentokorkeus      = etäisyysMaasta (ufonSijainti peli)

        laskeudu :: Ufo -> Ufo
        laskeudu ufo = ufo {
                -- Ufo maan suuntaiseksi
                ufo_teho=(maan_kulma, teho), 
                -- HACK miten saada ufo pysäytetyksi, muttei liimattua maahan
                ufo_liike=(kulma, max (teho-3) 0)
            }


varmistaTörmäysNavettaan :: Ufolifter -> Pelitila
varmistaTörmäysNavettaan peli = 
    let 
        törmääkö = törmääköNavettaa (ufoTörmäysviivat (taso_ufo peli)) (taso_navetat peli)
    in case törmääkö of
        False -> GameOn peli
        True -> GameOver peli "Törmäsit navettaan"


varmistaTörmäysKarikkoon :: Ufolifter -> Pelitila
varmistaTörmäysKarikkoon peli = 
    let 
        törmääkö = törmääköKarikkoon (ufoTörmäysviivat (taso_ufo peli)) (taso_karikot peli)
    in case törmääkö of
        False -> GameOn peli
        True -> GameOver peli "Törmäsit esteeseen"


päivitäPelitila :: Float -> Pelitila -> Pelitila
-- | Introssa ei päivitetä kuin kumulatiivinen aika
päivitäPelitila aika (Intro peli) = Intro peli{taso_aika=taso_aika peli + aika}


-- | Päivitä pelitilanne, ja varmista ettei ajauduta kuolettavaan tilanteeseen.
päivitäPelitila aika (GameOn peli) =
    let
        frame = päivitäPeli aika peli
    in
        GameOn (frame)
            >>== varmistaTörmäysKarikkoon
            -- >>== varmistaTörmäysNavettaan

päivitäPelitila aika (GameOver peli syy) = GameOver (päivitäPeli 0 peli) syy


-- | Päivitä pelitilanne.
päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika peli = 
        let
            edellinen_aika  = taso_aika peli
            (x, y)          = ufonSijainti peli
            (k,v)           = ufonTeho peli
            liike           = ufonLiike peli

            sijainti = (x, y) #+ (pisteVektorista liike)

            --(uusi_kulma, uusi_teho) = lisääPainovoima (lisääLiike liike teho aika) (x,y) aika
            (uusi_kulma, uusi_teho) = lisääLiike liike (k,-1*v) aika

            -- Lisää dragia
            uusi_liike = trace (show ((k,v), liike, (uusi_kulma, uusi_teho))) (uusi_kulma, uusi_teho) -- *(1-(0.95*aika)))

            lehmät = map (päivitäLehmä aika) (case ufo_traktori (taso_ufo peli) of
                    Päällä -> imeLehmiä (x,y) k (taso_lehmät peli)
                    Pois   -> taso_lehmät peli
                )

            -- Päiväitä lehmät
            -- (pisteet, lehmät) = foldr (\lehmä (p,l) ->
            --     let
            --         lehmä_etäisyys = pisteEtäisyys (lehmä_sijainti lehmä) (x,y)
            --     in
            --     if lehmä_etäisyys < 25 then
            --         trace "Lehmä poimittu" (p + 1, l)
            --     else
            --         (p, lehmä:l)
            --     ) (taso_pisteet peli, []) (map (päivitäLehmä aika) (taso_lehmät peli))
        in
            peli {
                taso_aika = aika + edellinen_aika, 
                -- taso_pisteet = pisteet,
                taso_ufo = (taso_ufo peli) {
                    ufo_sijainti=sijainti, ufo_liike=uusi_liike
                },
                taso_lehmät = lehmät
            }


-- | Pelin pääpiirtofunktio.
--   Tässä on paljon meneillään:
--    - Ufolle piirretään oikeaan sijaintiin ja kulmaan
--    - Skeneä skaalataan niin että maa sekä ufo näkyvät
--    - Piirretään pelimaailma
--    - Piirretään tracktrobeam ufon sijaintiin ja kulmaan.
--    - Ufolle piirretään varjo maanpinnalle, jonka koko ja etäisyys vaihtelee ufon etäisyyden mukaan.
piirräPeli :: Pelitila -> Picture
piirräPeli (Intro peli) = intro peli
piirräPeli (GameOn peli) = 
        let
            aika               = taso_aika peli
            (x, y)             = ufonSijainti peli
            (p_kulma, p_voima) = ufonTeho peli
            (m_kulma, m_voima) = ufonLiike peli
            etäisyys_maasta    = etäisyysMaasta (ufonSijainti peli)

            ufo = translate x y $ rotate (-(radToDeg p_kulma) + 90) $ lautanen aika

            -- abs on jäänne debukkauksesta. Todellisuudessa aluksen ei koskaan tulisi työntyä maan sisään
            kameran_zoom = min 2 $ sqrt (ikkunan_korkeus peli / (abs (etäisyys_maasta) + 40) * 0.8)
            --kameran_zoom = 0.1

            -- Maan grafiikasta hitboxia isompi.
            maa         = color (makeColor 0 1 0 1) (circleSolid (maanpinnan_taso*1.01))
            ilmakehä    = (color (light blue) $ circle (maanpinnan_taso * 1.2))

            piirräTraktori = 
                let
                    max_pituus = 200
                    pituus = min max_pituus etäisyys_maasta
                    sprite = translate x y $ rotate (-radToDeg (p_kulma+pi/2)) $ translate 0 (pituus/2) $ scale 1 (pituus/max_pituus) $ traktoriSprite
                in
                    case (ufo_traktori (taso_ufo peli)) of
                        Päällä -> sprite
                        Pois   -> blank

            -- Varjo joka seuraa ufoa maassa.
            varjo = let
                    ((ufo_vasen,ufo_ylä), (ufo_oikea,ufo_ala)) = ufo_mitat
                    korkeus         = abs ufo_ylä + abs ufo_ala
                    kerroin         = max 0 1-(etäisyys_maasta / (maanpinnan_taso * 0.15))

                    -- Laske varjon sijainti ufon sijainnista
                    (kulma,_)       = vektoriPisteestä (x,y)
                    (v_x, v_y)      = pisteVektorista (kulma, (maanpinnan_taso - (120 - korkeus/2) + 120*kerroin))

                    -- Laske varjon koko ufon koosta ja etäisyydestä maahan
                    varjon_koko     = (abs ufo_vasen + ufo_oikea) * kerroin

                    varjo_grafiikka = scale 1 0.3 $ color (makeColor 0 0.1 0 kerroin) $ circleSolid (varjon_koko/2)
                in
                    translate v_x v_y $ rotate (-radToDeg (p_kulma-pi/2)) $ varjo_grafiikka


            debuggausta = let
                ((va,oa), (va1,oa1)) = ufoTörmäysviivat (taso_ufo peli)
                
                törmäys 
                    | törmääköNavettaa ((va,oa), (va1,oa1)) (taso_navetat peli) = translate x y $ color red $ circle 100
                    | otherwise = blank

                navetat_apu = Pictures $ map (\navetta ->
                     let
                        sijainti = (navetta_sijainti navetta, 0)
                        a = sijainti #+ fst navetta_mitat
                        b = sijainti #+ snd navetta_mitat
                    in 
                        line $ koordinaatiKaarella [a,b]
                    ) (taso_navetat peli)
                in
                    color red $ Pictures [
                        line [va,oa], line [va1,oa1], -- UFON viivat
                        navetat_apu,
                        törmäys
                    ]

            skene = Pictures [
                    maa,
                    ilmakehä,
                    varjo,
                    piirräKarikot (taso_karikot peli),
                    piirräLehmät (taso_lehmät peli),
                    Pictures $ map (piirräNavetta) (taso_navetat peli),
                    piirräTraktori,
                    ufo,
                    debuggausta
                ]

            hud = scale 0.2 0.2 (
                    text ("Teho: " <> (show $ round p_voima)  <> " Kulma: " <> (show $ (round $ radToDeg (p_kulma))))
                )
                <> (translate (5) (ikkunan_korkeus peli - 20 ) $ scale 0.1 0.1 $ text $ "PISTEET: " <> show (taso_pisteet peli))
                <> translate (ikkunan_leveys peli - 50) (50) (
                    color white (
                            circle 20
                            <> line [(0,-22),(0,22)] <> line [(-22,0),(22,0)]
                        )
                    <> rotate (-radToDeg m_kulma) (scale (m_voima/10) 1 (color red (translate (10) 0 $ rectangleSolid 20 3)))
                )

        in
            Pictures [
                tähtiTaivas peli,
                (scale kameran_zoom kameran_zoom (scale 0.5 0.5 (translate (-x) (-y) (translate 0 (80) skene)))),

                scanLines peli,
                -- HUD
                translate (-(ikkunan_leveys peli)/2) (-(ikkunan_korkeus peli)/2) (color white hud)
            ]

piirräPeli (GameOver peli syy) = piirräPeli (GameOn peli)
        <> (color (makeColor 0 0 0 0.4) $ rectangleSolid (ikkunan_leveys peli) (ikkunan_korkeus peli))
        <> (scale 0.3 0.3 $ color white $ text $ toString syy)

-- | Luo skybox, joka muuttaa intensiteettiä ufon korkeuden mukaan
tähtiTaivas :: Ufolifter -> Picture
tähtiTaivas peli =
    let
        tähtiä              = 120

        -- Tummenna taivasta mitä korkeammalla lennetään.
        w                   = ikkunan_leveys peli
        h                   = ikkunan_korkeus peli
        etäisyys_maasta     = abs (etäisyysMaasta (ufonSijainti peli))
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

        -- Heiluta ufoa näennäisen epätasaisesti.
        ufo_x = w * sin(aika * 1.2) * 0.1
        ufo_y = h * sin(aika * 0.8) * 0.1
        ufo_kulma = (ufo_x - w) * (-0.5)
    in
        Pictures (map f (map kasvata (take viivoja [1..])))
        <> (translate ufo_x ufo_y $ rotate ufo_kulma $ scale 0.5 0.5 $ lautanen aika)
        -- <> (translate (-w/2 + 60) (h/2 + 20) introTitleSprite)
        <> translate (-300) (200) introTitleSprite
        <> (translate (-60) (-(h/2)+20) $ scale 0.15 0.15  $ color spacen_väri $ text "[ SPACE ]")
        <> scanLines peli


alustaTaso :: Ufolifter
alustaTaso =
    let
                navetat         = luoNavettoja 12
                karikot         = luoKarikkoja 40
                lehmät          = luoLehmiä 80
    in Peli
            {
                taso_aika       = 0,
                taso_pisteet    = 0,

                taso_ufo        = alustaUfo,

                taso_navetat    = navetat,
                taso_lehmät     = lehmät,
                taso_karikot    = karikot,

                -- Ikkunan leveys ja korkeus asetetaan tapahtumassa.
                ikkunan_leveys  = -1, 
                ikkunan_korkeus = -1
            }

alustaUfo :: Ufo
alustaUfo = Ufo {
        -- Voima tehossa ja liikkeessä perustuu ufon sulavaan laskeutumiseen
        ufo_sijainti = (0,5700),
        ufo_teho     = (pi*0.5, 0),
        ufo_liike    = (pi*1.5, 0),

        --ufo_teho     = (pi/2, 4),
        --ufo_liike    = (pi*1.5, 18),
        ufo_traktori = Pois
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
        10
        (Intro alustaTaso)
        piirräPeli
        reagoi
        päivitäPelitila
