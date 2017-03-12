module Texture where

import Vec3

data Texture = ConstantTexture Vec3 | CheckeredTexture Texture Texture deriving Show 

textureValue :: Texture -> Vec3 -> Vec3
textureValue (ConstantTexture c) _ = c 
textureValue (CheckeredTexture t0 t1) (px, py, pz) = 
    let sines = (sin (10 * px)) * (sin (10 * py)) * (sin (10 * pz)) 
    in if sines < 0 then
           textureValue t0 (px, py, pz)
       else
           textureValue t1 (px, py, pz)
