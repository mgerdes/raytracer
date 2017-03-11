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
import BVHTree

hitableType :: ((Int, Int), Int) -> Hitable
hitableType ((x, y), rand) 
    | t < 0.7   = Sphere { sphereCenter = (x', 0.2, y'),  
                           sphereRadius = 0.2,
                           sphereMaterial = Lambertian (r, g, b) }
    | otherwise = Sphere { sphereCenter = (x', 0.2, y'),  
                           sphereRadius = 0.2,
                           sphereMaterial = Metal (r, g, b) }
    where (dx, r0) = randomR (0.0, 0.9) (mkStdGen rand)
          (dy, r1) = randomR (0.0, 0.9) r0
          (r, r2) = randomR (0.0, 1.0) r1
          (g, r3) = randomR (0.0, 1.0) r2
          (b, r4) = randomR (0.0, 1.0) r3
          (t, r5) = randomR (0.0, 1.0) r4 :: (Double, StdGen)
          x' = (fromIntegral x) + dx
          y' = (fromIntegral y) + dy

hitables :: [Hitable]
hitables = map hitableType (zip [(x, y) | x <- [-10..10], y <- [-10..10]] (randoms (mkStdGen 100)))
            ++ [ Sphere { sphereCenter = (0.0, 1.0, 0.0),
                          sphereRadius = 1.0,
                          sphereMaterial = Lambertian (0.16, 0.67, 0.53) },

                 Sphere { sphereCenter = (-4.0, 1.0, 0.0),
                          sphereRadius = 1.0,
                          sphereMaterial = Metal (0.70, 0.75, 0.71) },
                        
                 Sphere { sphereCenter = (4.0, 1.0, 0.0),
                          sphereRadius = 1.0,
                          sphereMaterial = Metal (0.87, 0.72, 0.53) },

                 Sphere { sphereCenter = (0.0, -1000.0, 0.0),
                          sphereRadius = 1000.0,
                          sphereMaterial = Lambertian (0.8, 0.8, 0.8) } ]

bvh :: BVHTree
bvh = createBVHTree hitables (mkStdGen 10)

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
        let hitRec = bvhIntersect bvh ray
        in case hitRec of
             Just hr   -> colorHitRec hr ray depth
             Nothing   -> backGroundColor ray

width :: Int
width = 800

height :: Int
height = 400

cameraFrom :: Vec3
cameraFrom = (-7.0, 2.0, 2.5)

cameraTo :: Vec3
cameraTo = (0.0, 0.0, 0.0)

camera :: Camera
camera = createCamera cameraFrom 
                      cameraTo 
                      (0.0, 1.0, 0.0)
                      40.0 
                      ((fromIntegral width) / (fromIntegral height))
                      0.05
                      (distance cameraFrom cameraTo)

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed)

func' :: Double -> Double -> Vec3
func' x y = 
    let u = 1.0 - (x / (fromIntegral width))
        v = 1.0 - (y / (fromIntegral height))
    in color (createRay camera u v) 0 

func :: Int -> Int -> PixelRGB8
func x y = 
    let ns = 10
        xs = take ns (map (\r -> (fromIntegral x) + r) (randomList y))
        ys = take ns (map (\r -> (fromIntegral y) + r) (randomList x))
        colors = zipWith func' xs ys
        (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
    in PixelRGB8 (round r) (round g) (round b)

main :: IO()
main = writePng "out.png" (generateImage func width height)
