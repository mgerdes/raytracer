module Texture where

import Codec.Picture

import Vec3

data Texture = ConstantTexture Vec3 
             | CheckeredTexture Texture Texture 
             | ImageTexture (Image PixelRGB8)

textureValue :: Texture -> Vec3 -> Vec2 -> Vec3

textureValue (ConstantTexture c) _ _ = c 

textureValue (CheckeredTexture t0 t1) (px, py, pz) hitTexCoord = 
    let sines = (sin (10 * px)) * (sin (10 * py)) * (sin (10 * pz)) 
    in if sines < 0 then
           textureValue t0 (px, py, pz) hitTexCoord
       else
           textureValue t1 (px, py, pz) hitTexCoord

textureValue (ImageTexture img) _ (u, v) = 
    let width = imageWidth img
        height = imageHeight img
        i = round ((max (min u 1.0) 0.0) * ((fromIntegral width) - 1.0))
        j = round ((max (min v 1.0) 0.0) * ((fromIntegral height) - 1.0))
        (PixelRGB8 pr pg pb) = pixelAt img i j
    in ((fromIntegral pr) / 255.0, (fromIntegral pg) / 255.0, (fromIntegral pb) / 255.0)
