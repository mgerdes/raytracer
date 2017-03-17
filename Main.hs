import Codec.Picture
import System.Random
import Prelude hiding ((<*>))

import Hitables
import Texture
import HitRecord
import Camera
import Ray
import Vec3
import Material
import BVHTree

width :: Int
width = 800

height :: Int
height = 800

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

rayTraceImage :: String -> [Hitable] -> IO()
rayTraceImage fileName hitables =  
    let bvh = createBVHTree hitables (mkStdGen 10)

        colorBackground :: Ray -> Vec3
        colorBackground _ = 
            (0.0, 0.0, 0.0)

        colorHitRec :: HitRec -> Ray -> Int -> Vec3
        colorHitRec hr rayIn depth = 
            let maybeScatter = scatter (hrMaterial hr) rayIn (hrPosition hr) (hrNormal hr) (hrTexCoord hr)
                (ex, ey, ez) = materialEmitted (hrMaterial hr) (hrPosition hr)
            in 
                if depth > 50 then
                    (ex, ey, ez)
                else
                    case maybeScatter of
                         Just (rayOut, (ax, ay, az)) -> 
                            let (cx, cy, cz) = colorRay rayOut (depth + 1)
                            in (ex, ey, ez) <+> (ax * cx, ay * cy, az * cz)
                         Nothing -> (ex, ey, ez)

        colorRay :: Ray -> Int -> Vec3
        colorRay ray depth = 
            let maybeHr = bvhIntersect bvh ray
            in case maybeHr of
                Just hr -> colorHitRec hr ray depth
                Nothing -> colorBackground ray

        colorPixel' :: Double -> Double -> Vec3
        colorPixel' x y = 
            let u = x / (fromIntegral width)
                v = 1.0 - (y / (fromIntegral height))
            in colorRay (createRay camera u v) 0 

        colorPixel :: Int -> Int -> PixelRGB8
        colorPixel x y = 
            let ns = 1000
                xs = take ns (map (\rand -> (fromIntegral x) + rand) (randoms (mkStdGen y)))
                ys = take ns (map (\rand -> (fromIntegral y) + rand) (randoms (mkStdGen x)))
                colors = zipWith colorPixel' xs ys
                (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
            in PixelRGB8 (round r) (round g) (round b)

    in do writePng fileName (generateImage colorPixel width height)

readTexture :: String -> IO (Image PixelRGB8)
readTexture fileName = do
    eitherImg <- readImage fileName
    case eitherImg of
         Right dynamicImg -> return (convertRGB8 dynamicImg)

main :: IO()
main = do 
    earthTexture <- readTexture "earth.jpg"
    rayTraceImage "out.png" 
        [ 
            Translate {
                translateOffset = (130.0, 0.0, 65.0),
                translateHitable = RotateY {
                    rotateYCos = cos (-0.314),
                    rotateYSin = sin (-0.314),
                    rotateYHitable = createBox (0.0, 0.0, 0.0) (165.0, 165.0, 165.0) (Lambertian (ConstantTexture (0.73, 0.73, 0.73))) 
            }},

            Translate {
                translateOffset = (130.0 + 0.5 * 165.0, 165.0 + 50.0, 65.0 + 0.5 * 165.0),
                translateHitable = 
                    Sphere {
                        sphereCenter = (0.0, 0.0, 0.0),
                        sphereRadius = 50.0,
                        sphereMaterial = Lambertian (ImageTexture earthTexture)
                    }
            },

            Translate {
                translateOffset = (265.0, 0.0, 295.0),
                translateHitable = RotateY { 
                    rotateYCos = cos 0.261,
                    rotateYSin = sin 0.261,
                    rotateYHitable = createBox (0.0, 0.0, 0.0) (165.0, 330.0, 165.0) (Metal (ConstantTexture (0.73, 0.73, 0.73))) 
            }},

            FlipNormals YZRect { 
                yzRect_y0 = 0, yzRect_y1 = 555,
                yzRect_z0 = 0, yzRect_z1 = 555,
                yzRect_x = 555,
                yzRectMaterial = Lambertian (ConstantTexture (0.22, 0.72, 0.22)) 
            },

            YZRect { 
                yzRect_y0 = 0, yzRect_y1 = 555,
                yzRect_z0 = 0, yzRect_z1 = 555,
                yzRect_x = 0,
                yzRectMaterial = Lambertian (ConstantTexture (0.72, 0.22, 0.22)) 
            },

            XZRect {
                xzRect_x0 = 113, xzRect_x1 = 443,
                xzRect_z0 = 127, xzRect_z1 = 432,
                xzRect_y = 554,
                xzRectMaterial = DiffuseLight (ConstantTexture (4.0, 4.0, 4.0)) 
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
            }
        ]
