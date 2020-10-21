module Main where

import Prelude hiding (Down)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Data.Fixed

tausta :: Color
tausta = blue

ikkunan_leveys :: Int
ikkunan_leveys = 600
ikkunan_korkeus :: Int
ikkunan_korkeus = 400

data Ufolifter = Peli
    Float           -- Aika
    (Float,Float)   -- Sijainti
    Float           -- Teho
    Float           -- Akseli

lautanen :: Float -> Picture
lautanen aika = color väri (translate 0 korkeus keho)
    where
        -- Ufon paramtrejä
        väri = greyN 0.9
        leveys = 100
        korkeus = 40
        nopeus = 3

        lisääPallura offset
            | luku <= ositus || luku >= ositus * 3 = pallura
            | otherwise = blank
            where
                ositus = pi*2/4
                luku = (aika * nopeus + offset) `mod'` (pi*2)
                skaala = cos(luku)
                sijainti = sin(luku) * (-1)
                pallura = translate (sijainti*leveys-sijainti) 0 (scale skaala 1 $ color tausta (circleSolid 10))

        runko = (scale 1.0 0.4 (circleSolid leveys))
            <> translate 0 (korkeus * 0.85) (scale 1.0 0.6 (circleSolid korkeus)) -- Ohjaamo
            <> translate 0 (korkeus * (-0.9)) (scale 1.0 0.2 ( -- Imutusreikä
                circleSolid korkeus
                <> (color (light yellow) (circleSolid (korkeus*0.9)))
            ))
        -- Lisää pyörivät pallurat
        keho = foldr (\n runko' -> runko' <> lisääPallura (pi*2/6*n)) runko [1..6]


maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))


muutaTehoa :: Float -> Ufolifter -> Ufolifter
muutaTehoa teho peli = case peli of
    Peli aika paikka vanha_teho kulma -> Peli aika paikka (vanha_teho + teho) kulma

kallista :: Float -> Ufolifter -> Ufolifter
kallista kulma peli = case peli of
    Peli aika paikka teho vanha_kulma -> Peli aika paikka teho (vanha_kulma + kulma)

voimavektori :: Float -> Float -> (Float, Float)
voimavektori teho kulma = rotateV (- degToRad kulma) (0, teho)

reagoi :: Event -> Ufolifter -> Ufolifter
reagoi tapahtuma peli = case tapahtuma of
    EventKey (Char 'w') Down _ _ -> muutaTehoa 4 peli
    EventKey (Char 's') Down _ _ -> muutaTehoa (-4) peli 
    EventKey (Char 'a') Down _ _ -> kallista (-2) peli 
    EventKey (Char 'd') Down _ _ -> kallista 2 peli 
    _ -> peli

päivitäPeli :: Float -> Ufolifter -> Ufolifter
päivitäPeli aika edellinen = case edellinen of
    Peli edellinen_aika (x,y) teho kulma -> let
            (vx, vy) = voimavektori teho kulma
            sijainti = (x + aika * vx,
                max (-40) (y + aika * vy))
        in Peli (aika + edellinen_aika) sijainti teho kulma

piirräPeli :: Ufolifter -> Picture
piirräPeli peli = case peli of
    (Peli aika (x, y) _ kulma) -> let
                                ufo = translate x y $ rotate kulma $ lautanen aika
                                -- Varjo joka seuraa ufoa maassa.
                                varjo = translate x (-160 + (120*kerroin)) $ scale (1.0*kerroin) (0.4*kerroin) $ varjo_grafiikka
                                    where
                                        wh = fromIntegral ikkunan_korkeus*2 :: Float
                                        kerroin = max 0 ((wh - y )/ wh)
                                        varjo_grafiikka = color (makeColor 0 0 0 kerroin) (circleSolid 100)
                                skene = maa
                                    <> varjo
                                    <> ufo
                                    -- <> scale 100 100 (color white (Line (tähti 5)))
                            in                                
                                scale 0.5 0.5 (translate 0 (-180) skene)
                                <> scanLines

{-
    Tee ikkunan korkuinen ja levyinen elementti, joka peittää osan kuvasta
-}
scanLines :: Picture
scanLines = let
    joka_nth = 2
    väri = makeColor 0 0 0 0.6
    x = fromIntegral ikkunan_leveys :: Float
    korkeus = fromIntegral ikkunan_korkeus :: Float
    rivejä = floor (korkeus * 2 / joka_nth)
    viiva rivi = color väri (line [(-(x/2), y), (x/2, y)])
        where y = -korkeus + (rivi * joka_nth)
    in foldr (\n buffer -> buffer <> viiva n) blank (take rivejä [0..])

tähti ::[(Float, Float)]
tähti = let
        astetta = pi * 2 / 5 * 2
        pisteytä = \seg pisteet -> let
                n = fromIntegral seg :: Float
                pos = (sin (astetta * n), cos (astetta * n))
            in  pos:pisteet
    in
        foldr pisteytä [] (take (5+1) [1..])

main :: IO ()
main = do 
    play
        (InWindow "SuperCowPower" (ikkunan_leveys,ikkunan_korkeus) (300,200))
        tausta
        24
        (Peli 0 (0,100) 0 0)
        piirräPeli
        reagoi
        päivitäPeli
