module Camera where

import Prelude hiding ((<*>))
import Vec3
import Ray

data Camera = Camera {
    camOrigin :: Vec3,
    camLowerLeft :: Vec3,
    camWidth :: Vec3,
    camHeight :: Vec3
} deriving Show

createRay :: Camera -> Double -> Double -> Ray
createRay cam u v = 
    let h = camHeight cam
        w = camWidth cam
        ll = camLowerLeft cam
        origin = camOrigin cam
        direction = normalize ((ll <+> (h <*> v) <+> (w <*> u)) <-> origin)
    in
        Ray { rayOrigin = origin, rayDirection = direction }

