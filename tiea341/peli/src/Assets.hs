module Assets where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

import Data.Fixed

-- Vakioita
puun_korkeus :: Num p => p
puun_korkeus = 100

navetan_korkeus :: Num p => p
navetan_korkeus = 80 

svgToVec :: Path -> Path
svgToVec path = foldr (\(a,b) loput -> (a, -1*b):loput ) [] path

svgToVecFollow :: Point -> Path -> Path
svgToVecFollow (a,b) [] = [(a,b)]
svgToVecFollow (a,b) ((x,y):xs) = (x+a, y+(-1.0 * b)):(x,y):xs


tähti :: [(Float,Float)]
tähti = [(15.924437,11.343439), ( 16.587742,16.962543), ( 11.724225,14.071089), ( 6.5851121,16.438328), ( 7.8321396,10.91934), ( 3.9926882,6.7632701), ( 9.6269102,6.2438026), ( 12.393112,1.3079712), ( 14.628225,6.5059104), ( 20.177283,7.6114687), (15.924437,11.343439)]


lautanen :: Float -> Picture
lautanen aika = color väri (translate 0 korkeus keho)
    where
        -- Ufon paramtrejä
        väri        = greyN 0.9
        leveys      = 60
        korkeus     = 25
        nopeus      = 3

        aika' = aika * 0.8

        lisääPallura offset
            | luku <= ositus || luku >= ositus * 3 = pallura
            | otherwise = blank
            where
                ositus = pi*2/4
                luku = (aika' * nopeus + offset) `mod'` (pi*2)
                skaala = cos(luku)
                sijainti = sin(luku) * (-1)
                pallura = translate (sijainti*leveys-sijainti) 0 (scale skaala 1 $ color blue (circleSolid (korkeus*0.3)))

        runko = (scale 1.0 0.4 (circleSolid leveys))
            <> translate 0 (korkeus * 0.85) (scale 1.0 0.6 (circleSolid korkeus)) -- Ohjaamo
            <> translate 0 (korkeus * (-0.9)) (scale 1.0 0.2 ( -- Imutusreikä
                circleSolid korkeus
                <> (color (light yellow) (circleSolid (korkeus*0.9)))
            ))
        -- Lisää pyörivät pallurat
        keho = foldr (\n runko' -> runko' <> lisääPallura (pi*2/6*n)) runko [1..6]


navettaSprite :: Picture
--navettaSprite = polygon $ svgToVec [(0,50),(50,0),(100,50),(100,70),(0,70)] -- ^ Etufakaadi
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


lehmäSprite :: Picture
lehmäSprite = Pictures [
            takaraaja [(8.32504, 40.32504), (12.32504, 40.32504), (12.32504, 26.32504), (8.32504, 26.32504), (8.32504, 40.32504)] , -- Takataka
            häntä [(4.32504, 6.32504), (0.32504000000000044, 6.32504), (0.32504000000000044, 24.32504), (2.3250400000000004, 24.32504), (2.3250400000000004, 8.325040000000001), (4.32504, 8.325040000000001), (4.32504, 6.32504)] , -- Häntä
            takaraaja [(40.32504, 26.32504), (40.32504, 40.32504), (42.32504, 40.32504), (42.32504, 26.32504), (40.32504, 26.32504)] , -- Etutaka
            keho [(4.32504, 6.32504), (4.32504, 42.32504), (8.325040000000001, 42.32504), (8.325040000000001, 26.32504), (42.32504, 26.32504), (42.32504, 42.32504), (46.32504, 42.32504), (46.32504, 26.32504), (50.32504, 26.32504), (52.32504, 6.325040000000001), (4.32504, 6.32504)] , 
            utareet [(10.32504, 24.32504), (10.32504, 28.32504), (18.32504, 28.32504), (18.32504, 24.32504), (10.32504, 24.32504)] , -- Utareet
            täplä [(16.32504, 8.32504), (16.32504, 18.32504), (34.32504, 18.32504), (34.32504, 8.325040000000001), (16.32504, 8.32504)] , -- Täplä
            täplä [(8.32504, 8.32504), (12.32504, 8.32504), (12.32504, 14.32504), (8.32504, 14.32504), (8.32504, 8.32504)] , -- Täplä
            täplä [(6.32504, 18.32504), (12.325040000000001, 18.32504), (12.325040000000001, 22.32504), (6.325040000000001, 22.32504), (6.32504, 18.32504)] , -- Täplä
            täplä [(38.32504, 18.32504), (38.32504, 24.32504), (44.32504, 24.32504), (44.32504, 18.32504), (38.32504, 18.32504)] , -- Täplä
            keho [(58.32504, 0.32504), (62.32504, 0.32504), (60.32504, 2.32504), (58.32504, 2.32504), (58.32504, 0.32504)] , -- Korva
            keho [(46.32504, 0.32504), (42.32504, 0.32504), (44.32504, 2.32504), (46.32504, 2.32504), (46.32504, 0.32504)] , -- Korva
            keho [(48.32504, 14.32504), (56.32504, 14.32504), (58.32504, 0.32503999999999955), (46.32504, 0.32503999999999955), (48.32504, 14.32504)] , -- Kallo
            -- TUNTEMATON CubicBezier(start=(54.32504+12.32504j), control1=(54.32504+12.32504j), control2=(54.32504+14.32504j), end=(54.32504+14.32504j))
            nenä [(50.32504, 14.32504), (50.32504, 12.32504), (54.32504, 14.32504), (50.32504, 14.32504)] , -- Nenä
            silmä [(48.32504, 4.32504), (48.32504, 8.325040000000001), (50.32504, 8.325040000000001), (50.32504, 4.325040000000001), (48.32504, 4.32504)] , -- Silmä
            silmä [(54.32504, 4.32504), (54.32504, 8.325040000000001), (56.32504, 8.325040000000001), (56.32504, 4.325040000000001), (54.32504, 4.32504)] -- Silmä

        ]
    where
        keho = color white . p
        silmä = color black . p
        häntä = color black . p
        takaraaja = color (greyN 0.3) . p
        täplä = color black . p
        nenä = color black . p
        utareet = color (makeColor 0.9 0.5 0.35 1) . p-- #d68051
        p = piirräSprite

puuSprite :: Picture
puuSprite = Pictures [
            color juuri $ p [(32.0, 122.0), (28.0, 134.0), (32.0, 136.0), (36.0, 142.0), (46.0, 142.0), (48.0, 140.0), (52.0, 140.0), (56.0, 136.0), (56.0, 130.0), (52.0, 124.0), (48.0, 108.0), (34.0, 108.0), (32.0, 122.0)], -- Juuri
            color havusto $ p [(40.0, 10.0), (32.0, 18.0), (38.0, 16.0), (32.0, 34.0), (22.0, 44.0), (26.0, 42.0), (22.0, 48.0), (28.0, 46.0), (10.0, 64.0), (26.0, 60.0), (8.0, 78.0), (12.0, 78.0), (6.0, 86.0), (14.0, 84.0), (12.0, 88.0), (16.0, 86.0), (6.0, 96.0), (12.0, 94.0), (0.0, 108.0), (10.0, 106.0), (6.0, 112.0), (24.0, 104.0), (18.0, 116.0), (28.0, 110.0), (26.0, 116.0), (36.0, 110.0), (40.0, 120.0), (44.0, 110.0), (60.0, 118.0), (58.0, 112.0), (64.0, 114.0), (62.0, 110.0), (68.0, 112.0), (68.0, 108.0), (86.0, 112.0), (68.0, 96.0), (80.0, 100.0), (72.0, 84.0), (80.0, 86.0), (72.0, 76.0), (80.0, 76.0), (66.0, 64.0), (76.0, 66.0), (72.0, 60.0), (78.0, 62.0), (62.0, 46.0), (70.0, 48.0), (56.0, 32.0), (54.0, 24.0), (60.0, 26.0), (54.0, 18.0), (60.0, 20.0), (50.0, 10.0), (46.0, 0.0), (40.0, 10.0)], -- Havusto
            color varjostus $ p [(38.0, 16.0), (42.0, 14.0), (42.0, 28.0), (48.0, 18.0), (54.0, 24.0), (54.0, 28.0), (48.0, 22.0), (42.0, 32.0), (38.0, 20.0), (36.0, 22.0), (38.0, 16.0)], -- Varjostus
            color varjostus $ p [(28.0, 46.0), (30.0, 44.0), (30.0, 50.0), (36.0, 46.0), (38.0, 56.0), (46.0, 44.0), (54.0, 52.0), (52.0, 40.0), (62.0, 50.0), (62.0, 46.0), (66.0, 56.0), (56.0, 50.0), (56.0, 58.0), (46.0, 48.0), (38.0, 60.0), (34.0, 50.0), (26.0, 54.0), (28.0, 48.0), (24.0, 50.0), (28.0, 46.0)], -- Varjostus
            color varjostus $ p [(26.0, 60.0), (30.0, 58.0), (28.0, 70.0), (28.0, 62.0), (22.0, 64.0), (26.0, 60.0)], -- Varjostus
            color varjostus $ p [(40.0, 64.0), (28.0, 76.0), (28.0, 82.0), (40.0, 66.0), (42.0, 76.0), (50.0, 64.0), (62.0, 80.0), (58.0, 64.0), (74.0, 76.0), (80.0, 76.0), (66.0, 64.0), (58.0, 60.0), (58.0, 70.0), (48.0, 60.0), (44.0, 72.0), (40.0, 64.0)], -- Varjostus
            color varjostus $ p [(12.0, 78.0), (20.0, 72.0), (14.0, 84.0), (14.0, 78.0), (12.0, 78.0)],
            color varjostus $ p [(16.0, 86.0), (20.0, 82.0), (20.0, 90.0), (28.0, 82.0), (32.0, 92.0), (36.0, 84.0), (38.0, 96.0), (42.0, 90.0), (46.0, 98.0), (48.0, 86.0), (54.0, 92.0), (52.0, 76.0), (66.0, 94.0), (68.0, 82.0), (72.0, 84.0), (76.0, 92.0), (70.0, 86.0), (68.0, 96.0), (72.0, 104.0), (58.0, 90.0), (60.0, 102.0), (54.0, 98.0), (56.0, 108.0), (48.0, 98.0), (48.0, 110.0), (42.0, 98.0), (36.0, 108.0), (36.0, 90.0), (30.0, 100.0), (28.0, 88.0), (18.0, 96.0), (18.0, 88.0), (14.0, 92.0), (12.0, 90.0), (16.0, 86.0)] -- Varjostu
        ]
    where
        juuri = makeColor 0.4 0.3 0.1 1
        havusto = makeColor 0.5 0.7 0.3 1
        varjostus = dark juuri
        p = piirräSprite


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


piirräSprite :: [Point] -> Picture
piirräSprite g = Pictures [polygon $ vec, color black $ line vec]
                    where vec = svgToVec g


-- | Satunnaistaulukko. Suoraan alkuperäisen doomin määrittelemänä.
satunnaisTaulukko :: Num a => [a]
satunnaisTaulukko = cycle [
    0,   8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66 ,
    74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36 ,
    95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188 ,
    52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224 ,
    149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242 ,
    145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0 ,
    175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235 ,
    25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113 ,
    94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75 ,
    136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196 ,
    135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113 ,
    80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241 ,
    24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224 ,
    145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95 ,
    28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226 ,
    71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36 ,
    17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106 ,
    197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136 ,
    120, 163, 236, 249 ]
