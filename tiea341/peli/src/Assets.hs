module Assets where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Data.List ((!!))
import Peli
import Ufo

import Data.Fixed
import Aritmetiikka

svgToVec :: Path -> Path
svgToVec path = foldr (\(a,b) loput -> (a, -1*b):loput ) [] path

svgToVecFollow :: Point -> Path -> Path
svgToVecFollow (a,b) [] = [(a,b)]
svgToVecFollow (a,b) ((x,y):xs) = (x+a, y+(-1.0 * b)):(x,y):xs


scanLineSprite :: Float -> Float -> Picture
scanLineSprite leveys korkeus =
    let
        joka_nth    = 2
        x           = leveys / 2

        väri        = makeColor 0 0.1 0 0.6
        rivejä      = floor (korkeus * 2 / joka_nth)
        viiva rivi  = color väri (line [(-x, y), (x, y)])
                        where y = -korkeus + (rivi * joka_nth)
    in
        Pictures (map viiva (take rivejä [0..]))


tähtiTaivas :: Float -> Float -> Point -> Picture
tähtiTaivas w h sijainti  =
    let
        tähtiä              = 120

        -- Tummenna taivasta mitä korkeammalla lennetään.
        etäisyys_maasta     = abs (etäisyysMaasta sijainti)
        ilmakehän_korkeus   = maanpinnan_taso * 0.2

        intensiteetti       = min 1 $ max 0 (1 - (etäisyys_maasta / ilmakehän_korkeus))
        tausta_väri         = makeColor 0 0 intensiteetti 1
        tausta              = color tausta_väri $ rectangleSolid w h

        lisää_tähti n = translate x y $ color väri $ circleSolid koko
            where
                x = (sattuma !! n * w) - w / 2
                y = (sattuma !! (n+1) * w) - w / 2

                r = sattuma !! (n+0)
                g = sattuma !! (n+1)
                b = sattuma !! (n+2)
                väri = makeColor r g b (1-intensiteetti-0.2)
                koko = r + g + b

        in
            tausta
            <> Pictures (map lisää_tähti (take tähtiä [0..] ))


piirräSprite :: [Point] -> Picture
piirräSprite g = Pictures [polygon $ vec, color black $ line vec]
                    where vec = svgToVec g
