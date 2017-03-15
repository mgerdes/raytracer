module Material where

import Ray
import Vec3
import Texture

data Material = Lambertian Texture |
                Metal Texture | 
                DiffuseLight Texture

scatter :: Material -> Ray -> Vec3 -> Vec3 -> Vec2 -> Maybe (Ray, Vec3)

scatter (Lambertian albedo) _ hitPosition hitNormal hitTexPosition =
    let p = hitPosition
        n = hitNormal

        -- Hack to create a somewhat random number
        (px, py, pz) = p
        (nx, ny, nz) = n
        kindaRand = 10000000.0 * (px + nx + py + ny + pz + nz)

        target = p <+> n <+> (randomInUnitSphere (round kindaRand))
        scattered = Ray { rayOrigin = p, rayDirection = (normalize (target <-> p)) }
    in Just (scattered, textureValue albedo hitPosition hitTexPosition) 

scatter (Metal albedo) rayIn hitPosition hitNormal hitTexPosition = 
    let rd = rayDirection rayIn
        p = hitPosition
        n = hitNormal

        scattered = Ray { rayOrigin = p, rayDirection = reflect rd n }
    in Just (scattered, textureValue albedo hitPosition hitTexPosition)

scatter (DiffuseLight _) _ _ _ _ = Nothing

materialEmitted :: Material -> Vec3 -> Vec3
materialEmitted (Lambertian _) _ = (0.0, 0.0, 0.0)
materialEmitted (Metal _) _ = (0.0, 0.0, 0.0)
materialEmitted (DiffuseLight t) p = textureValue t p (0.0, 0.0)
