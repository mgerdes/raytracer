module HitRecord where

import Vec3
import Material

data HitRec = HitRec {
    hrTime :: Double,
    hrPosition :: Vec3,
    hrNormal :: Vec3,
    hrMaterial :: Material
} deriving Show

instance Eq HitRec where
    x == y = hrTime x == hrTime y

instance Ord HitRec where
    x < y  = hrTime x < hrTime y
    x <= y = hrTime x <= hrTime y
