module Material where

import Ray
import Vec3

data Material = Lambertian Vec3 | Metal Vec3 deriving Show

scatter :: Material -> Ray -> Vec3 -> Vec3 -> (Ray, Vec3)
scatter (Lambertian albedo) rayIn hitPosition hitNormal =  
    let p = hitPosition
        n = hitNormal

        -- Hack to create a somewhat random number
        (px, py, pz) = p
        (nx, ny, nz) = n
        kindaRand = 10000000.0 * (px + nx + py + ny + pz + nz)

        target = p <+> n <+> (randomInUnitSphere (round kindaRand))
        scattered = Ray { rayOrigin = p, rayDirection = (normalize (target <-> p)) }
    in (scattered, albedo) 

scatter (Metal albedo) rayIn hitPosition hitNormal = 
    let ro = rayOrigin rayIn
        rd = rayDirection rayIn
        p = hitPosition
        n = hitNormal

        scattered = Ray { rayOrigin = p, rayDirection = reflect rd n }
    in (scattered, albedo)
