module Material where

import Ray
import Vec3
import Texture

import Prelude hiding ((<*>))

data Material = Lambertian Texture |
                Metal Texture Double | 
                DiffuseLight Texture deriving Show

scatter :: Material -> Ray -> Vec3 -> Vec3 -> Vec2 -> Maybe (Ray, Vec3)

scatter (Lambertian albedo) _ hitPosition hitNormal hitTexPosition =
    let p = hitPosition
        n = hitNormal

        -- Hack to create a somewhat random number
        (px, py, pz) = p
        (nx, ny, nz) = n
        kindaRand = 10000000.0 * (px + nx + py + ny + pz + nz)

        target = n <+> (randomInUnitSphere (round kindaRand))
        scattered = Ray { rayOrigin = p, rayDirection = normalize target }
    in Just (scattered, textureValue albedo hitPosition hitTexPosition) 

scatter (Metal albedo fuzz) rayIn hitPosition hitNormal hitTexPosition = 
    let rd = rayDirection rayIn
        p = hitPosition
        n = hitNormal
        
        -- Hack to create a somewhat random number
        (px, py, pz) = p
        (nx, ny, nz) = n
        kindaRand = 10000000.0 * (px + nx + py + ny + pz + nz)

        target = (reflect rd n) <+> ((randomInUnitSphere (round kindaRand)) <*> fuzz)
        scattered = Ray { rayOrigin = p, rayDirection = normalize target}
    in Just (scattered, textureValue albedo hitPosition hitTexPosition)

scatter (DiffuseLight _) _ _ _ _ = Nothing

materialEmitted :: Material -> Vec3 -> Vec3
materialEmitted (Lambertian _) _ = (0.0, 0.0, 0.0)
materialEmitted (Metal _ _) _ = (0.0, 0.0, 0.0)
materialEmitted (DiffuseLight t) p = textureValue t p (0.0, 0.0)
