import Codec.Picture
import System.Random
import Prelude hiding ((<*>))

import HitRecord
import Camera
import Ray
import Vec3
import Material
import BVHTree
import Scene

width :: Int
width = 400

height :: Int
height = 400

cameraFrom :: Vec3
cameraFrom = (278.0, 278.0, -800.0)

cameraTo :: Vec3
cameraTo = (278.0, 278.0, 0.0)

bvh :: BVHTree
bvh = createBVHTree hitables (mkStdGen 10)

colorHitRec :: HitRec -> Ray -> Int -> Vec3
colorHitRec hr rayIn depth = 
    let maybeScatter = scatter (hrMaterial hr) rayIn (hrPosition hr) (hrNormal hr)
        (ex, ey, ez) = materialEmitted (hrMaterial hr) (hrPosition hr)
    in 
        if depth > 50 then
            (ex, ey, ez)
        else
            case maybeScatter of
                 Just (rayOut, (ax, ay, az)) -> 
                    let (cx, cy, cz) = color rayOut (depth + 1)
                    in (ex, ey, ez) <+> (ax * cx, ay * cy, az * cz)
                 Nothing -> (ex, ey, ez)

color :: Ray -> Int -> Vec3
color ray depth = 
    let maybeHr = bvhIntersect bvh ray
    in case maybeHr of
        Just hr -> colorHitRec hr ray depth
        Nothing -> (0.0, 0.0, 0.0)

camera :: Camera
camera = createCamera cameraFrom 
                      cameraTo 
                      (0.0, 1.0, 0.0)
                      40.0 
                      ((fromIntegral width) / (fromIntegral height))
                      0.0
                      (distance cameraFrom cameraTo)

colorPixel' :: Double -> Double -> Vec3
colorPixel' x y = 
    let u = x / (fromIntegral width)
        v = 1.0 - (y / (fromIntegral height))
    in color (createRay camera u v) 0 

colorPixel :: Int -> Int -> PixelRGB8
colorPixel x y = 
    let ns = 1000
        xs = take ns (map (\rand -> (fromIntegral x) + rand) (randoms (mkStdGen y)))
        ys = take ns (map (\rand -> (fromIntegral y) + rand) (randoms (mkStdGen x)))
        colors = zipWith colorPixel' xs ys
        (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
    in PixelRGB8 (round r) (round g) (round b)

main :: IO()
main = writePng "out.png" (generateImage colorPixel width height)
