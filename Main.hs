import Codec.Picture
import Data.Maybe (isJust, catMaybes)
import System.Random
import Prelude hiding ((<*>))

import Hitables
import HitRecord
import Camera
import Ray
import Vec3
import Material

backGroundColor :: Ray -> Vec3
backGroundColor ray = 
    let (_, rdy, _) = rayDirection ray 
        t = 0.5 * rdy + 0.5
        r = (1.0 - t) + 0.5 * t
        g = (1.0 - t) + 0.7 * t
        b = (1.0 - t) + 1.0 * t
    in (r, g, b)

colorHitRec :: HitRec -> Ray -> Int -> Vec3
colorHitRec hr rayIn depth = 
    let (rayOut, (ax, ay, az)) = scatter (hrMaterial hr) rayIn (hrPosition hr) (hrNormal hr)
        (cx, cy, cz) = color rayOut (depth + 1)
    in (ax * cx, ay * cy, az * cz)

color :: Ray -> Int -> Vec3
color ray depth = 
    if depth > 50 
    then (0.0, 0.0, 0.0)
    else
        let hitRecs = catMaybes (map (intersect ray) hitables)
        in case hitRecs of
             []        -> backGroundColor ray
             otherwise -> colorHitRec (minimum hitRecs) ray depth

hitables :: [Hitable]
hitables = [
            Sphere { sphereCenter = (0.0, 0.0, -1.0),
                     sphereRadius = 0.5,
                     sphereMaterial = Lambertian (0.8, 0.3, 0.3) },

            Sphere { sphereCenter = (0.0, -100.5, -1.0),
                     sphereRadius = 100.0,
                     sphereMaterial = Lambertian (0.8, 0.8, 0.0) },
                   
            Sphere { sphereCenter = (1.0, 0.0, -1.0),
                     sphereRadius = 0.5,
                     sphereMaterial = Metal (0.8, 0.6, 0.2) },

            Sphere { sphereCenter = (-1.0, 0.0, -1.0),
                     sphereRadius = 0.5,
                     sphereMaterial = Metal (0.8, 0.8, 0.8) } 
           ]

camera :: Camera
camera = Camera { camOrigin = (0.0, 0.0, 0.0),
                  camLowerLeft = (-2.0, -1.0, -1.0),
                  camWidth = (4.0, 0.0, 0.0), 
                  camHeight = (0.0, 2.0, 0.0) }

width :: Int
width = 400

height :: Int
height = 200

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed)

func :: Int -> Int -> PixelRGB8
func x y = 
    let ns = 100
        xs = take ns (map (\r -> (fromIntegral x) + r) (randomList y))
        ys = take ns (map (\r -> (fromIntegral y) + r) (randomList x))
        colors = zipWith func' xs ys
        (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
    in 
        PixelRGB8 (round r) (round g) (round b)

func' :: Double -> Double -> Vec3
func' x y = 
    let u = 1.0 - (x / (fromIntegral width))
        v = 1.0 - (y / (fromIntegral height))
    in
        color (createRay camera u v) 0 

main :: IO()
main = writePng "out.png" (generateImage func width height)
