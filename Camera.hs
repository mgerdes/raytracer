module Camera where

import Prelude hiding ((<*>))
import Vec3
import Ray

data Camera = Camera {
    camOrigin :: Vec3,
    camLowerLeft :: Vec3,
    camWidth :: Vec3,
    camHeight :: Vec3,
    camLensRadius :: Double,
    camWAxis :: Vec3,
    camUAxis :: Vec3,
    camVAxis :: Vec3
} deriving Show

createCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Double -> Double -> Camera
createCamera from at up vfov aspect aperture focusDist = 
    let theta = vfov * pi / 180.0;
        halfHeight = tan(theta / 2.0) 
        halfWidth = aspect * halfHeight

        w = normalize (from <-> at)
        u = normalize (cross up w)
        v = cross w u

        origin = from
        lowerLeft = from <-> (u <*> (focusDist * halfWidth)) <-> (v <*> (focusDist * halfHeight)) <-> (w <*> focusDist)
        width = u <*> (2.0 * focusDist * halfWidth)
        height = v <*> (2.0 * focusDist * halfHeight)
        lensRadius = aperture * 0.5;
    in  Camera { camOrigin = origin,
                 camLowerLeft = lowerLeft,
                 camWidth = width,
                 camHeight = height, 
                 camLensRadius = lensRadius,
                 camWAxis = w,
                 camUAxis = u,
                 camVAxis = v }

createRay :: Camera -> Double -> Double -> Ray
createRay cam u v = 
    let o = camOrigin cam
        h = camHeight cam
        w = camWidth cam
        ll = camLowerLeft cam
        uAxis = camUAxis cam
        vAxis = camVAxis cam

        (rdx, rdy, _) = (randomInUnitSphere (round (10000000.0 * (u + v)))) <*> (camLensRadius cam)
        offset = (uAxis <*> rdx) <+> (vAxis <*> rdy)
        origin = o <+> offset
        direction = normalize ((ll <+> (h <*> v) <+> (w <*> u)) <-> o <-> offset)
    in Ray { rayOrigin = origin, rayDirection = direction }
