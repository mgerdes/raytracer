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
width = 400

height :: Int
height = 400

cameraFrom :: Vec3
cameraFrom = (0.0, 8.0, -10.0)

cameraTo :: Vec3
cameraTo = (0.0, 1.0, 0.0)

camera :: Camera
camera = createCamera cameraFrom 
                      cameraTo 
                      (0.0, 1.0, 0.0)
                      20.0 
                      ((fromIntegral width) / (fromIntegral height))
                      0.0
                      (distance cameraFrom cameraTo)

rayTraceImage :: String -> [Hitable] -> IO()
rayTraceImage fileName hitables =  
    let bvh = createBVHTree hitables (mkStdGen 10)

        colorBackground :: Ray -> Vec3
        colorBackground ray = 
            let (_, rdy, _) = rayDirection ray 
                t = 0.5 * rdy + 0.5
                r = (1.0 - t) + 0.5 * t
                g = (1.0 - t) + 0.7 * t
                b = (1.0 - t) + 1.0 * t
            in (r, g, b)

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
            let ns = 100
                xs = take ns (map (\rand -> (fromIntegral x) + rand) (randoms (mkStdGen y)))
                ys = take ns (map (\rand -> (fromIntegral y) + rand) (randoms (mkStdGen x)))
                colors = zipWith colorPixel' xs ys
                (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
            in PixelRGB8 (round r) (round g) (round b)

    in do writePng fileName (generateImage colorPixel width height)

main :: IO()
main = do 
    eitherImg <- readImage "earth.jpg"
    case eitherImg of
        Left err -> putStrLn err
        Right dynamicImg -> 
            let img = convertRGB8 dynamicImg
                hitables = [
                    Sphere {
                        sphereCenter = (0.0, 1.0, 0.0),
                        sphereRadius = 1.0,
                        sphereMaterial = Lambertian (ImageTexture img)
                    },
                
                    Sphere { 
                        sphereCenter = (-2.2, 1.0, 0.0),
                        sphereRadius = 1.0,
                        sphereMaterial = Metal (ConstantTexture (0.70, 0.75, 0.71)) 
                    },
                            
                    Sphere { 
                        sphereCenter = (2.2, 1.0, 0.0),
                        sphereRadius = 1.0,
                        sphereMaterial = Metal (ConstantTexture (0.87, 0.72, 0.53)) 
                    },

                    Sphere {
                        sphereCenter = (0.0, -1000.0, 0.0),
                        sphereRadius = 1000.0,
                        sphereMaterial = Lambertian (ConstantTexture (0.8, 0.8, 0.8)) 
                    } ]
            in do rayTraceImage "out.png" hitables
