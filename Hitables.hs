module Hitables where

import Prelude hiding ((<*>))
import Data.Maybe

import Vec3
import Ray
import HitRecord
import Material

data Hitable = Sphere { 
    sphereCenter :: Vec3,
    sphereRadius :: Double,
    sphereMaterial :: Material
} | XYRect {
    xyRect_x0 :: Double,
    xyRect_x1 :: Double,
    xyRect_y0 :: Double,
    xyRect_y1 :: Double,
    xyRect_z :: Double,
    xyRectMaterial :: Material
} | XZRect {
    xzRect_x0 :: Double,
    xzRect_x1 :: Double,
    xzRect_z0 :: Double,
    xzRect_z1 :: Double,
    xzRect_y :: Double,
    xzRectMaterial :: Material
} | YZRect {
    yzRect_y0 :: Double,
    yzRect_y1 :: Double,
    yzRect_z0 :: Double,
    yzRect_z1 :: Double,
    yzRect_x :: Double,
    yzRectMaterial :: Material
} | FlipNormals {
    hitable :: Hitable
} | Box {
    boxMin :: Vec3,
    boxMax :: Vec3,
    side0 :: Hitable, 
    side1 :: Hitable, 
    side2 :: Hitable, 
    side3 :: Hitable, 
    side4 :: Hitable, 
    side5 :: Hitable 
} deriving Show

createBox :: Vec3 -> Vec3 -> Material -> Hitable
createBox (p0x, p0y, p0z) (p1x, p1y, p1z) mat = 
    Box { boxMin = (p0x, p0y, p0z),
          boxMax = (p1x, p1y, p1z),
          side0 = XYRect p0x p1x p0y p1y p1z mat,
          side1 = FlipNormals (XYRect p0x p1x p0y p1y p0z mat),
          side2 = XZRect p0x p1x p0z p1z p1y mat,
          side3 = FlipNormals (XZRect p0x p1x p0z p1z p0y mat),
          side4 = YZRect p0y p1y p0z p1z p1x mat,
          side5 = FlipNormals (YZRect p0y p1y p0z p1z p0x mat) }

hitableIntersect :: Hitable -> Ray -> Maybe HitRec
hitableIntersect Sphere { sphereCenter = so,
                          sphereRadius = sr,
                          sphereMaterial = mat } 
                 ray =
    let ro = rayOrigin ray
        rd = rayDirection ray

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
            Just HitRec { hrTime = t, 
                          hrPosition = position, 
                          hrNormal = normal,
                          hrMaterial = mat }

hitableIntersect XYRect { xyRect_x0 = x0,
                          xyRect_x1 = x1,
                          xyRect_y0 = y0,
                          xyRect_y1 = y1,
                          xyRect_z = z,
                          xyRectMaterial = mat }
                 ray =
    let (_, _, roz) = rayOrigin ray
        (_, _, rdz) = rayDirection ray
        t = ((z - roz) / rdz) - 0.001

        position = pointAtTime ray t
        (x, y, _) = position
        normal = (0.0, 0.0, 1.0)
    in
        if t < 0 || x < x0 || x > x1 || y < y0 || y > y1 then
            Nothing
        else
            Just HitRec { hrTime = t,
                          hrPosition = position,
                          hrNormal = normal,
                          hrMaterial = mat }

hitableIntersect XZRect { xzRect_x0 = x0,
                          xzRect_x1 = x1,
                          xzRect_z0 = z0,
                          xzRect_z1 = z1,
                          xzRect_y = y,
                          xzRectMaterial = mat }
                 ray =
    let (_, roy, _) = rayOrigin ray
        (_, rdy, _) = rayDirection ray
        t = ((y - roy) / rdy) - 0.001

        position = pointAtTime ray t
        (x, _, z) = position
        normal = (0.0, 1.0, 0.0)
    in
        if t < 0 || x < x0 || x > x1 || z < z0 || z > z1 then
            Nothing
        else
            Just HitRec { hrTime = t,
                          hrPosition = position,
                          hrNormal = normal,
                          hrMaterial = mat }

hitableIntersect YZRect { yzRect_y0 = y0,
                          yzRect_y1 = y1,
                          yzRect_z0 = z0,
                          yzRect_z1 = z1,
                          yzRect_x = x,
                          yzRectMaterial = mat }
                 ray =
    let (rox, _, _) = rayOrigin ray
        (rdx, _, _) = rayDirection ray
        t = ((x - rox) / rdx) - 0.001

        position = pointAtTime ray t
        (_, y, z) = position
        normal = (1.0, 0.0, 0.0)
    in
        if t < 0 || y < y0 || y > y1 || z < z0 || z > z1 then
            Nothing
        else
            Just HitRec { hrTime = t,
                          hrPosition = position,
                          hrNormal = normal,
                          hrMaterial = mat }

hitableIntersect FlipNormals { hitable = h } ray =
    let maybeHr = hitableIntersect h ray
    in case maybeHr of
        Just hr -> Just HitRec { hrTime = hrTime hr,
                                 hrPosition = hrPosition hr,
                                 hrNormal = (hrNormal hr) <*> (-1.0),
                                 hrMaterial = hrMaterial hr }
        Nothing -> Nothing

hitableIntersect Box { side0 = s0, side1 = s1, 
                       side2 = s2, side3 = s3, 
                       side4 = s4, side5 = s5 }
                 ray =
    let hrs = catMaybes (map (\h -> hitableIntersect h ray) [s0, s1, s2, s3, s4, s5])
    in case hrs of
        [] -> Nothing
        hrs' -> Just (minimum hrs')
