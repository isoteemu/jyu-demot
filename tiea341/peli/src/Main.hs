module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle

import Data.List (partition)
import Peli
import Assets
import Aritmetiikka
import Intro
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
        - Liikkeissä käytetään napakoordinaatteja. Syynä koska.
        - Satunnaisluvut ovat Doomista kopioitu lista lukuja. Tarjoaa riittävän satunnaisuuden, mutta ei tarvitse
          huolehtia satunnaislukujen ikävästä taipumuksesta olla satunnaisia. Doctor Whon sanoin:
            "Satunnaisuuden suuri puute on ettei se suosi hyviä vaihtoehtoja yli huonojen."

    Bugeja:
        - Lehmät ovat navettojen takana piilossa. Niiden olisi ollut tarkoitus vaeltaa.
        - Liike ei välitä pelin pysäyttämisetä.
        - Glossin polygon piirtää jotain väärin.
        - Spritejen nollapiste on vasen alakulma.
-}

data Pelitila = GameOver Ufolifter Text | GameOn Ufolifter | Intro Ufolifter deriving Show

data Ufolifter = Peli {
    taso_aika       :: Float,
    taso_tulos      :: Natural,

    taso_ufo        :: Ufo,

    taso_navetat    :: [Navetta],
    taso_lehmät     :: [Lehmä],
    taso_karikot    :: [Karikko],

    ikkunan_leveys  :: Float,
    ikkunan_korkeus :: Float

} deriving(Show)


alustaUfo :: Ufo
alustaUfo = Ufo {
        -- Voima tehossa ja liikkeessä perustuu ufon sulavaan laskeutumiseen
        ufo_sijainti = (0, 5700),

        ufo_teho     = (pi/2, -4),
        ufo_liike    = (pi*1.5, 11),
        ufo_traktori = Pois
    }


alustaTaso :: Ufolifter
alustaTaso =
    let
                navetat         = luoNavettoja 12
                karikot         = luoKarikkoja 40
                lehmät          = luoLehmiä 80
                -- TODO: Sijoita lehmät abouttiarallaa navettojen kohdalle

    in Peli
            {
                taso_aika       = 0,
                taso_tulos      = 0,

                taso_ufo        = alustaUfo,

                taso_navetat    = navetat,
                taso_lehmät     = lehmät,
                taso_karikot    = karikot,

                -- Ikkunan leveys ja korkeus asetetaan tapahtumassa.
                ikkunan_leveys  = -1, 
                ikkunan_korkeus = -1
            }


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
    in case tila of
        GameOn peli         -> GameOn peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}
        GameOver peli syy   -> GameOver peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus} syy
        Intro peli          -> Intro peli{ikkunan_leveys=leveys, ikkunan_korkeus=korkeus}

-- | Käynnistä peli millä tahansa näppäimellä introissa
reagoi tapahtuma (Intro peli) = case tapahtuma of
    EventKey _ Down _ _                     -> GameOn peli
    _                                       -> Intro peli

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
                            if (abs laskeutumis_kulma) <= 0.2 && momentti < 1 then
                                -- Ufo saavutti turvallisesti maan tason.
                                GameOn peli
                            else
                                trace ("Törmäys maahan: " <> show momentti <> " @ " <> show laskeutumis_kulma ) 
                                    $ GameOver peli "Törmäsit maahan"

        -- Lentely jatkuu normaalisti
        | otherwise = GameOn peli 
    where
        (_, momentti)   = ufonLiike peli
        (kulma, teho)   = ufonTeho peli
        maan_kulma      = pisteKulma (ufonSijainti peli) (0,0)

        laskeutumis_kulma = (maan_kulma - kulma)
        lentokorkeus      = etäisyysMaasta (ufonSijainti peli)


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
            >>== varmistaTörmäysNavettaan
            >>== varmistaEtäisyys

päivitäPelitila aika (GameOver peli syy) = GameOver (päivitäPeli 0 peli) syy


-- | Päivitä pelitilanne.
päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika peli = 
        let
            edellinen_aika  = taso_aika peli
            (x, y)          = ufonSijainti peli
            (k, v)          = ufonTeho peli
            liike           = ufonLiike peli

            -- Päivitä ENSIN sijainti, sen jälkeen laske uudet voimat.
            sijainti = (x, y) #+ (pisteVektorista liike)
            (uusi_kulma, uusi_teho) = lisääPainovoima (lisääLiike liike (k,-1*v) aika) (x,y) aika

            -- Lisää dragia
            uusi_liike = (uusi_kulma, uusi_teho * ilmanvastus)

            -- Jaa lehmät kahteen; Poimittuihin ja vapaana kulkeviin
            (poimittu, lehmät) = partition (nouseekoKyytiin (x,y)) (
                    map (päivitäLehmä aika) (case ufo_traktori (taso_ufo peli) of
                        Päällä -> imeLehmiä (x,y) k aika (taso_lehmät peli)
                        Pois   -> taso_lehmät peli
                    )
                )

            tulos = taso_tulos peli + genericLength poimittu

            -- | Onko lehmä riittävän lähellä ufoa
            nouseekoKyytiin :: Point -> Lehmä -> Bool
            nouseekoKyytiin ufon_sijainti lehmä =
                let
                    koko = sqrt ((leveys' lehmä_mitat) * (korkeus' lehmä_mitat) * 0.8)
                    etäisyys = pisteEtäisyys (lehmä_sijainti lehmä) ufon_sijainti
                in etäisyys < koko

        in
            peli {
                taso_aika       = aika + edellinen_aika,
                taso_tulos      = tulos,
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

            skene = Pictures [
                    maa,
                    ilmakehä,
                    varjo,
                    piirräKarikot (taso_karikot peli),
                    piirräLehmät (taso_lehmät peli),
                    Pictures $ map (piirräNavetta) (taso_navetat peli),
                    piirräTraktori,
                    ufo
                ]

            hud = scale 0.2 0.2 (
                    text ("Teho: " <> (show $ round p_voima)  <> " Kulma: " <> (show $ (round $ radToDeg (p_kulma))))
                )
                <> (translate (5) (ikkunan_korkeus peli - 20 ) $ scale 0.1 0.1 $ text $ "PISTEET: " <> show (taso_tulos peli))
                <> translate (ikkunan_leveys peli - 50) (50) (
                    color white (
                            circle 20
                            <> line [(0,-22),(0,22)] <> line [(-22,0),(22,0)]
                        )
                    <> rotate (-radToDeg m_kulma) (scale (m_voima/10) 1 (color red (translate (10) 0 $ rectangleSolid 20 3)))
                )

        in
            Pictures [
                piirräTähtiTaivas peli,
                (scale kameran_zoom kameran_zoom (scale 0.5 0.5 (translate (-x) (-y) (translate 0 (80) skene)))),

                scanLines peli,
                -- HUD
                translate (-(ikkunan_leveys peli)/2) (-(ikkunan_korkeus peli)/2) (color white hud)
            ]


piirräPeli (GameOver peli syy) = piirräPeli (GameOn peli)
        <> (color (makeColor 0 0 0 0.4) $ rectangleSolid (ikkunan_leveys peli) (ikkunan_korkeus peli))
        <> (translate (-150) 0 $ scale 0.3 0.3 $ color white $ text $ toString syy)

-- | Luo skybox, joka muuttaa intensiteettiä ufon korkeuden mukaan
--   Olisi hyvä yleistää, mutta riippuu niin monesta peli -seikasta että
piirräTähtiTaivas :: Ufolifter -> Picture
piirräTähtiTaivas peli = tähtiTaivas (ikkunan_leveys peli) (ikkunan_korkeus peli) (ufonSijainti peli)


-- | Tee ikkunan korkuinen ja levyinen elementti, joka peittää osan kuvasta viivoilla.
scanLines :: Ufolifter -> Picture
scanLines peli = scanLineSprite (ikkunan_leveys peli) (ikkunan_korkeus peli)


-- | Intronäyttö
intro :: Ufolifter -> Picture
intro peli = introNäyttö (taso_aika peli) (ikkunan_leveys peli) (ikkunan_korkeus peli)
                <> scanLines peli


alustaPeli :: Pelitila
alustaPeli = GameOn alustaTaso

main :: IO ()
main = do
    play
        (InWindow "Ufolifter" (720,480) (300,5200))
        black
        30
        (Intro alustaTaso)
        piirräPeli
        reagoi
        päivitäPelitila
