import Codec.Picture
import Data.Maybe (isJust, catMaybes)
import System.Random
import Prelude hiding ((<*>))
import Hitables
import HitRecord
import Camera
import Ray
import Vec3

backGroundColor :: Ray -> Vec3
backGroundColor ray = 
    let (_, rdy, _) = rayDirection ray 
        t = 0.5 * rdy + 0.5
        r = (1.0 - t) + 0.5 * t
        g = (1.0 - t) + 0.7 * t
        b = (1.0 - t) + 1.0 * t
    in (r, g, b)

colorHitRec :: HitRec -> Vec3
colorHitRec hitRec = 
    let n = hrNormal hitRec
        p = hrPosition hitRec
        t = hrTime hitRec

        -- Super hacky way of generating a somewhat random number
        kindaRand = 1000000.0 * t

        target = p <+> n <+> (randomInUnitSphere (round kindaRand))
        ray = Ray p (normalize (target <-> p))
    in color ray <*> 0.5

color :: Ray -> Vec3
color ray = 
    let hitRecs = catMaybes (map (intersect ray) hitables)
    in case hitRecs of
         []        -> backGroundColor ray
         otherwise -> colorHitRec (minimum hitRecs)

hitables :: [Hitable]
hitables = [Sphere { sphereCenter = (0.0, 0.0, 0.0), sphereRadius = 0.5 },
            Sphere { sphereCenter = (0.0, -100.5, 0.0), sphereRadius = 100.0 }]

camera :: Camera
camera = Camera { camOrigin = (0.0, 5.0, 0.0),
                  camLowerLeft = (-2.0, 0.0, -1.0),
                  camWidth = (4.0, 0.0, 0.0), 
                  camHeight = (0.0, 0.0, 2.0) }

width :: Int
width = 400

height :: Int
height = 200

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed)

func :: Int -> Int -> PixelRGB8
func x y = 
    let ns = 10
        xs = take ns (map (\r -> (fromIntegral x) + r) (randomList y))
        ys = take ns (map (\r -> (fromIntegral y) + r) (randomList x))
        colors = zipWith func' xs ys
        (r, g, b) = (foldl (<+>) zero colors) <*> (255.0 / (fromIntegral ns))
    in 
        PixelRGB8 (round r) (round g) (round b)

func' :: Double -> Double -> Vec3
func' x y = 
    let u = 1.0 - (x / (fromIntegral width))
        v = 1.0 - (y / (fromIntegral height))
    in
        color (createRay camera u v) 

main = writePng "thing.png" (generateImage func width height)
