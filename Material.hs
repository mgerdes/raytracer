module Material where

import Ray
import Vec3
import Texture

data Material = Lambertian Texture | Metal Texture deriving Show

scatter :: Material -> Ray -> Vec3 -> Vec3 -> (Ray, Vec3)
scatter (Lambertian albedo) _ hitPosition hitNormal =  
    let p = hitPosition
        n = hitNormal

        -- Hack to create a somewhat random number
        (px, py, pz) = p
        (nx, ny, nz) = n
        kindaRand = 10000000.0 * (px + nx + py + ny + pz + nz)

        target = p <+> n <+> (randomInUnitSphere (round kindaRand))
        scattered = Ray { rayOrigin = p, rayDirection = (normalize (target <-> p)) }
    in (scattered, textureValue albedo hitPosition) 

scatter (Metal albedo) rayIn hitPosition hitNormal = 
    let rd = rayDirection rayIn
        p = hitPosition
        n = hitNormal

        scattered = Ray { rayOrigin = p, rayDirection = reflect rd n }
    in (scattered, textureValue albedo hitPosition)
