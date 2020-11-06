module Intro where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Aritmetiikka
import Data.List ((!!))
import Ufo
import Assets


introTitleSprite :: Picture
introTitleSprite = Pictures [
            väritä $ p [(130.0, 40.0), (156.0, 40.0), (156.0, 106.0), (216.0, 106.0), (216.0, 40.0), (242.0, 40.0), (242.0, 130.0), (130.0, 130.0), (130.0, 40.0)], -- UFO -> U
            väritä $ p [(248.0, 40.0), (248.0, 130.0), (274.0, 130.0), (274.0, 96.0), (340.0, 96.0), (340.0, 74.0), (274.0, 74.0), (274.0, 62.0), (340.0, 62.0), (340.0, 40.0), (248.0, 40.0)], -- UFO -> F
            väritä $ p [(344.0, 40.0), (472.0, 40.0), (472.0, 130.0), (344.0, 130.0), (344.0, 40.0)], -- UFO -> O
            reikä $ p [(370.0, 64.0), (446.0, 106.0), (370.0, 106.0), (370.0, 64.0)] -- UFO -> O reikä
        ] <> Pictures [
            väritä $ p [(128.0, 286.0), (144.0, 286.0), (144.0, 326.0), (184.0, 326.0), (184.0, 340.0), (128.0, 340.0), (128.0, 286.0)], -- LIFTER -> L
            väritä $ p [(272.0, 300.0), (272.0, 286.0), (332.0, 286.0), (332.0, 300.0), (308.0, 300.0), (308.0, 340.0), (292.0, 340.0), (292.0, 300.0), (272.0, 300.0)], 
            väritä $ p [(336.0, 286.0), (336.0, 340.0), (392.0, 340.0), (392.0, 326.0), (352.0, 326.0), (352.0, 318.0), (392.0, 318.0), (392.0, 306.0), (352.0, 306.0), (352.0, 300.0), (392.0, 300.0), (392.0, 286.0), (336.0, 286.0)], -- LIFTER -> E
            väritä $ p [(396.0, 300.0), (396.0, 286.0), (470.0, 286.0), (470.0, 324.0), (458.0, 324.0), (476.0, 340.0), (452.0, 340.0), (436.0, 326.0), (414.0, 326.0), (414.0, 340.0), (396.0, 340.0), (396.0, 312.0), (454.0, 312.0), (454.0, 300.0), (396.0, 300.0)], 
            väritä $ p [(190.0, 286.0), (190.0, 340.0), (206.0, 340.0), (206.0, 286.0), (190.0, 286.0)],
            väritä $ p [(212.0, 286.0), (212.0, 340.0), (228.0, 340.0), (228.0, 320.0), (266.0, 320.0), (266.0, 306.0), (228.0, 306.0), (228.0, 300.0), (266.0, 300.0), (266.0, 286.0), (212.0, 286.0)] -- LIFTER -> F
        ]
    where
        väritä = color (greyN 0.5)
        reikä = color green
        p g = color white $ line (svgToVec g)


-- | Intronäyttö
introNäyttö :: Float -> Float -> Float -> Picture
introNäyttö aika w h =
    let
        viivoja = 21 :: Int -- Poukkoilevien viivojen määrä
        viiva = (round aika)
        kasvata n = n + (fromIntegral viiva)

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
