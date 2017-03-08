module Hitables where

import Prelude hiding ((<*>))
import Vec3
import Ray
import HitRecord

data Hitable = Sphere {
    sphereCenter :: Vec3,
    sphereRadius :: Double
} deriving Show

intersect :: Ray -> Hitable -> Maybe HitRec
intersect ray sphere =
     let ro = rayOrigin ray
         rd = rayDirection ray
         so = sphereCenter sphere
         sr = sphereRadius sphere

         a = dot rd rd
         b = 2.0 * (dot rd (ro <-> so))
         c = (dot (ro <-> so) (ro <-> so)) - sr * sr
         det = b * b - 4 * a * c

         t1 = (-b + sqrt(det)) / (2.0 * a)
         t2 = (-b - sqrt(det)) / (2.0 * a)
         t = min t1 t2

         position = pointAtTime ray t
         normal = normalize (position <-> so)
     in
         if det < 0 || t < 0 then
             Nothing
         else
            Just HitRec { hrTime = t, hrPosition = position, hrNormal = normal }
