module Scene where

import System.Random

import Hitables
import Ray
import Vec3
import Material
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

backGroundColor :: Ray -> Vec3
backGroundColor ray = 
    let (_, rdy, _) = rayDirection ray 
        t = 0.5 * rdy + 0.5
        r = (1.0 - t) + 0.5 * t
        g = (1.0 - t) + 0.7 * t
        b = (1.0 - t) + 1.0 * t
    in (r, g, b)
