module HitRecord where

import Vec3

data HitRec = HitRec {
    hrTime :: Double,
    hrPosition :: Vec3,
    hrNormal :: Vec3
} deriving Show

instance Eq HitRec where
    x == y = hrTime x == hrTime y

instance Ord HitRec where
    x < y  = hrTime x < hrTime y
    x <= y = hrTime x <= hrTime y
