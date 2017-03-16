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
    flipNormalsHitable :: Hitable
} | Box {
    boxMin :: Vec3,
    boxMax :: Vec3,
    boxSide0 :: Hitable, 
    boxSide1 :: Hitable, 
    boxSide2 :: Hitable, 
    boxSide3 :: Hitable, 
    boxSide4 :: Hitable, 
    boxSide5 :: Hitable 
} | Translate {
    translateOffset :: Vec3,
    translateHitable :: Hitable
} | RotateY {
    rotateYCos :: Double,
    rotateYSin :: Double,
    rotateYHitable :: Hitable
} deriving Show

createBox :: Vec3 -> Vec3 -> Material -> Hitable
createBox (p0x, p0y, p0z) (p1x, p1y, p1z) mat = 
    Box { boxMin = (p0x, p0y, p0z),
          boxMax = (p1x, p1y, p1z),
          boxSide0 = XYRect p0x p1x p0y p1y p1z mat,
          boxSide1 = FlipNormals (XYRect p0x p1x p0y p1y p0z mat),
          boxSide2 = XZRect p0x p1x p0z p1z p1y mat,
          boxSide3 = FlipNormals (XZRect p0x p1x p0z p1z p0y mat),
          boxSide4 = YZRect p0y p1y p0z p1z p1x mat,
          boxSide5 = FlipNormals (YZRect p0y p1y p0z p1z p0x mat) }

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

        (px, py, pz) = (position <-> so) </> sr
        phi = atan2 pz px
        theta = asin py
        u = 1.0 - (phi + pi) / (2.0 * pi)
        v = (theta + (0.5 * pi)) / pi
        texCoord = (u, v)
    in 
        if det < 0 || t < 0 then
            Nothing
        else
            Just HitRec { hrTime = t, 
                          hrPosition = position, 
                          hrNormal = normal,
                          hrMaterial = mat,
                          hrTexCoord = texCoord }

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
                          hrMaterial = mat,
                          hrTexCoord = (0.0, 0.0) }

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
                          hrMaterial = mat, 
                          hrTexCoord = ((x - x0) / (x1 - x0), (z - z0) / (z1 - z0)) }

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
                          hrMaterial = mat,
                          hrTexCoord = (0.0, 0.0) }

hitableIntersect FlipNormals { flipNormalsHitable = h } ray =
    let maybeHr = hitableIntersect h ray
    in case maybeHr of
        Just hr -> Just HitRec { hrTime = hrTime hr,
                                 hrPosition = hrPosition hr,
                                 hrNormal = (hrNormal hr) <*> (-1.0),
                                 hrMaterial = hrMaterial hr,
                                 hrTexCoord = (0.0, 0.0) }
        Nothing -> Nothing

hitableIntersect Translate { translateHitable = h, translateOffset = o }
                 Ray { rayOrigin = ro, rayDirection = rd } =
    let r = Ray { rayOrigin = ro <-> o, rayDirection = rd }
        maybeHr = hitableIntersect h r
    in case maybeHr of
        Just hr -> Just HitRec { hrTime = hrTime hr,
                                 hrPosition = (hrPosition hr) <+> o,
                                 hrNormal = hrNormal hr,
                                 hrMaterial = hrMaterial hr,
                                 hrTexCoord = (0.0, 0.0) }
        Nothing -> Nothing

hitableIntersect RotateY { rotateYHitable = h, rotateYSin = s, rotateYCos = c }
                 Ray { rayOrigin = ro, rayDirection = rd } =
    let r = Ray { rayOrigin = rotateY ro c (-s), rayDirection = rotateY rd c (-s) }
        maybeHr = hitableIntersect h r
    in case maybeHr of
        Just hr -> Just HitRec { hrTime = hrTime hr,
                                 hrPosition = rotateY (hrPosition hr) c s,
                                 hrNormal = rotateY (hrNormal hr) c s,
                                 hrMaterial = hrMaterial hr, 
                                 hrTexCoord = hrTexCoord hr }
        Nothing -> Nothing

hitableIntersect Box { boxSide0 = s0, boxSide1 = s1, 
                       boxSide2 = s2, boxSide3 = s3, 
                       boxSide4 = s4, boxSide5 = s5 }
                 ray =
    let hrs = catMaybes (map (\h -> hitableIntersect h ray) [s0, s1, s2, s3, s4, s5])
    in case hrs of
        [] -> Nothing
        hrs' -> Just (minimum hrs')
