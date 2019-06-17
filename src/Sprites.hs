{-|
Module      : Sprites
Description : Module containig all vectorial sprites for haskHell 3D
-}
module Sprites where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

-- | heart sprite with size 13 x 11 pixels
heart :: Picture
heart = Scale 20 20 $ Pictures [ out, r1, r2, r3, r4, r5]
    where
        pixel = Polygon[(0,0), (0,1), (1,1), (1,0)]
        bP  = Color black pixel
        bR1 = Color (makeColor 1     0.855 0.753 1) pixel
        bR2 = Color (makeColor 0.878 0     0     1) pixel
        bR3 = Color (makeColor 0.996 0     0     1) pixel
        bR4 = Color (makeColor 0.992 0.153 0.153 1) pixel
        bR5 = Color (makeColor 0.996 0.29  0.294 1) pixel

        r1  = Pictures [ Translate 2  9 bR1, Translate 3  9 bR1
                       , Translate 4  8 bR1, Translate 5  7 bR1
                       , Translate 6  7 bR1, Translate 9  9 bR1
                       , Translate 10 8 bR1
                       ]
        r2  = Pictures [ Translate 6  1 bR2, Translate 5  2 bR2
                       , Translate 4  3 bR2, Translate 3  4 bR2
                       , Translate 2  5 bR2, Translate 1  6 bR2
                       , Translate 1  7 bR2
                       ]
        r3  = Pictures [ Translate 1  8 bR3, Translate 2  7 bR3
                       , Translate 3  6 bR3, Translate 4  5 bR3
                       , Translate 5  4 bR3, Translate 2  6 bR3
                       , Translate 3  5 bR3, Translate 4  4 bR3
                       , Translate 5  3 bR3, Translate 6  2 bR3
                       , Translate 6  3 bR3, Translate 6  4 bR3
                       , Translate 7  2 bR3, Translate 7  3 bR3
                       , Translate 7  4 bR3, Translate 8  3 bR3
                       , Translate 8  4 bR3, Translate 9  4 bR3
                       , Translate 9  5 bR3, Translate 10 5 bR3
                       , Translate 11 6 bR3, Translate 10 6 bR3
                       , Translate 11 7 bR3
                       ]
        r4  = Pictures [ Translate 2  8 bR4, Translate 3  7 bR4
                       , Translate 4  6 bR4, Translate 5  5 bR4
                       , Translate 6  5 bR4, Translate 7  5 bR4
                       , Translate 8  5 bR4, Translate 8  6 bR4
                       , Translate 9  6 bR4
                       ]
        r5  = Pictures [ Translate 3  8 bR5, Translate 4  7 bR5
                       , Translate 5  6 bR5, Translate 6  6 bR5
                       , Translate 7  6 bR5, Translate 4  9 bR5
                       , Translate 5  8 bR5, Translate 7  7 bR5
                       , Translate 8  7 bR5, Translate 9  7 bR5
                       , Translate 10 7 bR5, Translate 11 8 bR5
                       , Translate 10 9 bR5, Translate 9  8 bR5
                       , Translate 8  8 bR5, Translate 7  8 bR5
                       , Translate 8  9 bR5
                       ]
        out = Pictures [ Translate 6  0  bP, Translate 5  1  bP
                       , Translate 7  1  bP, Translate 4  2  bP
                       , Translate 8  2  bP, Translate 3  3  bP
                       , Translate 9  3  bP, Translate 2  4  bP
                       , Translate 10 4  bP, Translate 1  5  bP
                       , Translate 11 5  bP, Translate 12 6  bP
                       , Translate 0  6  bP, Translate 12 7  bP
                       , Translate 0  7  bP, Translate 12 8  bP
                       , Translate 0  8  bP, Translate 1  9  bP
                       , Translate 11 9  bP, Translate 2  10 bP
                       , Translate 3  10 bP, Translate 4  10 bP
                       , Translate 8  10 bP, Translate 9  10 bP
                       , Translate 10 10 bP, Translate 7  9  bP
                       , Translate 6  8  bP, Translate 5  9  bP
                       ]

