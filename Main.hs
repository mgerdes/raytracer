import Codec.Picture
import System.Random
import Prelude hiding ((<*>))

import Hitables
import HitRecord
import Camera
import Ray
import Vec3
import Material
import BVHTree
import Texture

hitableType :: ((Int, Int), Int) -> Hitable
hitableType ((x, y), rand) 
    | t < 0.7   = Sphere { sphereCenter = (x', 0.2, y'),  
                           sphereRadius = 0.2,
                           sphereMaterial = Lambertian (ConstantTexture (r, g, b)) }
    | otherwise = Sphere { sphereCenter = (x', 0.2, y'),  
                           sphereRadius = 0.2,
                           sphereMaterial = Metal (ConstantTexture (r, g, b)) }
    where (dx, r0) = randomR (0.0, 0.9) (mkStdGen rand)
          (dy, r1) = randomR (0.0, 0.9) r0
          (r, r2) = randomR (0.0, 1.0) r1
          (g, r3) = randomR (0.0, 1.0) r2
          (b, r4) = randomR (0.0, 1.0) r3
          (t, _) = randomR (0.0, 1.0) r4 :: (Double, StdGen)
          x' = (fromIntegral x) + dx
          y' = (fromIntegral y) + dy

hitables :: [Hitable]
hitables = [ 
    Translate {
        translateOffset = (130.0, 0.0, 65.0),
        translateHitable = RotateY { 
            rotateYCos = cos (-0.314),
            rotateYSin = sin (-0.314),
            rotateYHitable = createBox (0.0, 0.0, 0.0) (165.0, 165.0, 165.0) (Lambertian (ConstantTexture (0.73, 0.73, 0.73))) 
    }},

    Translate {
        translateOffset = (265.0, 0.0, 295.0),
        translateHitable = RotateY { 
            rotateYCos = cos 0.261,
            rotateYSin = sin 0.261,
            rotateYHitable = createBox (0.0, 0.0, 0.0) (165.0, 330.0, 165.0) (Lambertian (ConstantTexture (0.73, 0.73, 0.73))) 
    }},

    FlipNormals YZRect { 
        yzRect_y0 = 0, yzRect_y1 = 555,
        yzRect_z0 = 0, yzRect_z1 = 555,
        yzRect_x = 555,
        yzRectMaterial = Lambertian (ConstantTexture (0.12, 0.45, 0.15)) 
    },

    YZRect { 
        yzRect_y0 = 0, yzRect_y1 = 555,
        yzRect_z0 = 0, yzRect_z1 = 555,
        yzRect_x = 0,
        yzRectMaterial = Lambertian (ConstantTexture (0.65, 0.05, 0.05)) 
    },

    XZRect {
        xzRect_x0 = 113, xzRect_x1 = 443,
        xzRect_z0 = 127, xzRect_z1 = 432,
        xzRect_y = 554,
        xzRectMaterial = DiffuseLight (ConstantTexture (7.0, 7.0, 7.0)) 
    },

    XZRect { 
        xzRect_x0 = 0, xzRect_x1 = 555,
        xzRect_z0 = 0, xzRect_z1 = 555,
        xzRect_y = 0,
        xzRectMaterial = Lambertian (ConstantTexture (0.73, 0.73, 0.73)) 
    },

    FlipNormals XZRect { 
        xzRect_x0 = 0, xzRect_x1 = 555,
        xzRect_z0 = 0, xzRect_z1 = 555,
        xzRect_y = 555,
        xzRectMaterial = Lambertian (ConstantTexture (0.73, 0.73, 0.73)) 
    },

     FlipNormals XYRect { 
        xyRect_x0 = 0, xyRect_x1 = 555,
        xyRect_y0 = 0, xyRect_y1 = 555,
        xyRect_z = 555,
        xyRectMaterial = Lambertian (ConstantTexture (0.73, 0.73, 0.73)) 
     } ]

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

width :: Int
width = 400

height :: Int
height = 400

cameraFrom :: Vec3
cameraFrom = (278.0, 278.0, -800.0)

cameraTo :: Vec3
cameraTo = (278.0, 278.0, 0.0)

camera :: Camera
camera = createCamera cameraFrom 
                      cameraTo 
                      (0.0, 1.0, 0.0)
                      40.0 
                      ((fromIntegral width) / (fromIntegral height))
                      0.0
                      (distance cameraFrom cameraTo)

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed)

func' :: Double -> Double -> Vec3
func' x y = 
    let u = x / (fromIntegral width)
        v = 1.0 - (y / (fromIntegral height))
    in color (createRay camera u v) 0 

func :: Int -> Int -> PixelRGB8
func x y = 
    let ns = 100
        xs = take ns (map (\rand -> (fromIntegral x) + rand) (randomList y))
        ys = take ns (map (\rand -> (fromIntegral y) + rand) (randomList x))
        colors = zipWith func' xs ys
        (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
    in PixelRGB8 (round r) (round g) (round b)

main :: IO()
main = writePng "out.png" (generateImage func width height)
