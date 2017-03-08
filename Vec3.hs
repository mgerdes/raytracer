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
(x, y, z) </> s = ((s / x), (s / y), (s / z))

normalize :: Vec3 -> Vec3
normalize v =  v <*> (1.0 / sqrt (dot v v))

dot :: Vec3 -> Vec3 -> Double 
dot (x0, y0, z0) (x1, y1, z1) = x0 * x1 + y0 * y1 + z0 * z1

cross :: Vec3 -> Vec3 -> Vec3
cross (x0, y0, z0) (x1, y1, z1) = (y0 * z1 - z0 * y1, z0 * x1 - x0 * z1, x0 * y1 - y0 * x1)

zero :: Vec3
zero = (0.0, 0.0, 0.0)

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
