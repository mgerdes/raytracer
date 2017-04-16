import Codec.Picture
import Codec.Picture.Types
import System.Random
import Debug.Trace
import Prelude hiding ((<*>))

import Hitables
import Texture
import HitRecord
import Camera
import Ray
import Vec3
import Material
import BVHTree
import Window
import RayTraceModel

width :: Int
width = 800

height :: Int
height = 400

cameraFrom :: Vec3
cameraFrom = (150.0, 50.0, 50.0)

cameraTo :: Vec3
cameraTo = (100.0, 50.0, 500.0)

camera :: Camera
camera = createCamera cameraFrom 
                      cameraTo 
                      (0.1, 1.0, 0.0)
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
            let ns = 1
                xs = take ns (map (\rand -> (fromIntegral x) + rand) (randoms (mkStdGen y)))
                ys = take ns (map (\rand -> (fromIntegral y) + rand) (randoms (mkStdGen x)))
                colors = zipWith colorPixel' xs ys
                (r, g, b) = (gammaCorrect ((foldl (<+>) zero colors) </> (fromIntegral ns))) <*> 255.0
            in PixelRGB8 (round r) (round g) (round b)

        updatePixel :: Int -> Int -> Int -> PixelRGB8 -> PixelRGB8
        updatePixel ns x y (PixelRGB8 r g b) = 
            let ns' = (fromIntegral ns) :: Double
                rx = fst (random (mkStdGen (10000 * ns * y))) :: Double
                ry = fst (random (mkStdGen (10000 * ns * x))) :: Double
                x' = fromIntegral x :: Double
                y' = fromIntegral y :: Double
                r' = fromIntegral r :: Double
                g' = fromIntegral g :: Double
                b' = fromIntegral b :: Double
                (ncr, ncg, ncb) = colorPixel' (x' + rx) (y' + ry)   
                (ocr, ocg, ocb) = (r' / 255.0, g' / 255.0, b' / 255.0)
                ncr' = sqrt(((ocr * ocr) * ns' + ncr) / (ns' + 1.0)) * 255.0
                ncg' = sqrt(((ocg * ocg) * ns' + ncg) / (ns' + 1.0)) * 255.0
                ncb' = sqrt(((ocb * ocb) * ns' + ncb) / (ns' + 1.0)) * 255.0
            in PixelRGB8 (round ncr') (round ncg') (round ncb')

        updateModel :: RayTraceModel -> RayTraceModel
        updateModel model = 
            trace (show (modelNumSamples model))
            RayTraceModel { 
                modelNumSamples = (modelNumSamples model) + 1,
                modelImage = pixelMapXY (updatePixel (modelNumSamples model)) (modelImage model)
            }

        initialModel :: RayTraceModel
        initialModel = 
            RayTraceModel {
                modelNumSamples = 0, 
                modelImage = generateImage (\_ _ -> PixelRGB8 0 0 0) width height
            }

    in do drawImage updateModel initialModel -- (generateImage colorPixel width height)

readTexture :: String -> IO (Image PixelRGB8)
readTexture fileName = do
    eitherImg <- readImage fileName
    case eitherImg of
         Right dynamicImg -> return (convertRGB8 dynamicImg)

lightColors :: [Vec3]
lightColors = [ (144.0 / 255.0, 48.0 / 255.0, 90.0 / 255.0),
                (48.0 / 255.0, 60.0 / 255.0, 116.0 / 255.0),
                (170.0 / 255.0, 137.0 / 255.0, 57.0 / 255.0),
                (112.0 / 255.0, 156.0 / 255.0, 52.0 / 255.0) ]

main :: IO()
main = do 
    earthTexture <- readTexture "earth.jpg"
    rayTraceImage "out.png" 
        ((map 
            (\i -> XYRect {
                        xyRect_x0 = 0, xyRect_x1 = 555, 
                        xyRect_y0 = 30 * i, xyRect_y1 = 30 * i + 5,
                        xyRect_z = 554,
                        xyRectMaterial = DiffuseLight (ConstantTexture (lightColors !! (mod (round i) 4))) 
                   }) 
            [1..19]) 
        ++ 
        (map 
            (\i -> YZRect {
                        yzRect_y0 = 30 * i, yzRect_y1 = 30 * i + 5, 
                        yzRect_z0 = 0, yzRect_z1 = 555,
                        yzRect_x = 1,
                        yzRectMaterial = DiffuseLight (ConstantTexture (lightColors !! (mod (round i) 4))) 
                   }) 
            [1..19]) 
        ++ 
        (map 
            (\i -> YZRect {
                        yzRect_y0 = 30 * i, yzRect_y1 = 30 * i + 5, 
                        yzRect_z0 = 0, yzRect_z1 = 555,
                        yzRect_x = 554,
                        yzRectMaterial = DiffuseLight (ConstantTexture (lightColors !! (mod (round i) 4))) 
                   }) 
            [1..19]) 
        ++ 
        (map 
            (\i -> XYRect {
                        xyRect_x0 = 0, xyRect_x1 = 555, 
                        xyRect_y0 = 30 * i, xyRect_y1 = 30 * i + 5,
                        xyRect_z = 1,
                        xyRectMaterial = DiffuseLight (ConstantTexture (lightColors !! (mod (round i) 4))) 
                   }) 
            [1..19]) 
        ++ 
        (map 
            (\i -> XZRect {
                        xzRect_x0 = 50, xzRect_x1 = 505, 
                        xzRect_z0 = 100 * i + 30, xzRect_z1 = 100 * i + 60,
                        xzRect_y = 554,
                        xzRectMaterial = DiffuseLight (ConstantTexture (1.0, 1.0, 1.0)) 
                   }) 
            [1..4]) 
        ++
        [ 
            Translate {
                translateOffset = (125.0, 75.0, 450.0),
                translateHitable = 
                    Sphere {
                        sphereCenter = (0.0, 0.0, 0.0),
                        sphereRadius = 75.0,
                        sphereMaterial = Metal (ConstantTexture (0.18, 0.19, 0.18)) 0.0
                    }
            },

            FlipNormals YZRect { 
                yzRect_y0 = 0, yzRect_y1 = 555,
                yzRect_z0 = 0, yzRect_z1 = 555,
                yzRect_x = 555,
                yzRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2))
            },

            YZRect { 
                yzRect_y0 = 0, yzRect_y1 = 555,
                yzRect_z0 = 0, yzRect_z1 = 555,
                yzRect_x = 0,
                yzRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2))
            },

            XZRect { 
                xzRect_x0 = 0, xzRect_x1 = 555,
                xzRect_z0 = 0, xzRect_z1 = 555,
                xzRect_y = 0,
                xzRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2))
            },

            FlipNormals XZRect { 
                xzRect_x0 = 0, xzRect_x1 = 555,
                xzRect_z0 = 0, xzRect_z1 = 555,
                xzRect_y = 555,
                xzRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2))
            },

            FlipNormals XYRect { 
                xyRect_x0 = 0, xyRect_x1 = 555,
                xyRect_y0 = 0, xyRect_y1 = 555,
                xyRect_z = 555,
                xyRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2))
            },

            XYRect { 
                xyRect_x0 = 0, xyRect_x1 = 555,
                xyRect_y0 = 0, xyRect_y1 = 555,
                xyRect_z = 0,
                xyRectMaterial = Lambertian (ConstantTexture (0.2, 0.2, 0.2)) 
            }
        ])
