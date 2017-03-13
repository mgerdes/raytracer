module Vec3 where

import System.Random
import Prelude hiding ((<*>))

type Vec3 = (Double, Double, Double)

(<+>) :: Vec3 -> Vec3 -> Vec3
(x0, y0, z0) <+> (x1, y1, z1) = ((x0 + x1), (y0 + y1), (z0 + z1))

(<->) :: Vec3 -> Vec3 -> Vec3
(x0, y0, z0) <-> (x1, y1, z1) = ((x0 - x1), (y0 - y1), (z0 - z1))

(<*>) :: Vec3 -> Double -> Vec3 
(x, y, z) <*> s = ((s * x), (s * y), (s * z))

(</>) :: Vec3 -> Double -> Vec3 
(x, y, z) </> s = ((x / s), (y / s), (z / s))

normalize :: Vec3 -> Vec3
normalize v =  v <*> (1.0 / sqrt (dot v v))

dot :: Vec3 -> Vec3 -> Double 
dot (x0, y0, z0) (x1, y1, z1) = x0 * x1 + y0 * y1 + z0 * z1

distance :: Vec3 -> Vec3 -> Double
distance v0 v1 = sqrt (dot (v0 <-> v1) (v0 <-> v1))

cross :: Vec3 -> Vec3 -> Vec3
cross (x0, y0, z0) (x1, y1, z1) = (y0 * z1 - z0 * y1, z0 * x1 - x0 * z1, x0 * y1 - y0 * x1)

zero :: Vec3
zero = (0.0, 0.0, 0.0)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v <-> (n <*> (2 * (dot v n)))

gammaCorrect :: Vec3 -> Vec3
gammaCorrect (r, g, b) = 
    let r' = min (max (sqrt r) 0.0) 1.0
        g' = min (max (sqrt g) 0.0) 1.0
        b' = min (max (sqrt b) 0.0) 1.0
    in (r', g', b')

-- http://mathworld.wolfram.com/DiskPointPicking.html
-- Returns a point in the unit sphere in the x-y plane
randomInUnitDisk :: Int -> Vec3
randomInUnitDisk seed = 
    let r0 = randomR (0.0, 1.0) (mkStdGen seed)
        r1 = randomR (0.0, 2.0 * pi) (snd r0)
        r = sqrt (fst r0)
        theta = fst r1
    in  (r * cos(theta), r * sin(theta), 0.0)

-- http://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability
randomInUnitSphere :: Int -> Vec3
randomInUnitSphere seed = 
    let r0 = randomR (-1.0, 1.0) (mkStdGen seed)
        r1 = randomR (-1.0, 1.0) (snd r0)
        r2 = randomR (-1.0, 1.0) (snd r1)
        r3 = randomR (0.0, 1.0) (snd r2)
        x = fst r0
        y = fst r1
        z = fst r2
        u = fst r3
    in (normalize (x, y, z)) <*> (u ** (1.0 / 3.0))
