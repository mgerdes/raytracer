module Ray where

import Prelude hiding ((<*>))

import Vec3

data Ray = Ray {
    rayOrigin :: Vec3,
    rayDirection :: Vec3
} deriving Show
         
pointAtTime :: Ray -> Double -> Vec3
pointAtTime ray t = (rayOrigin ray) <+> ((rayDirection ray) <*> t) 
