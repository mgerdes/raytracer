module RayTraceModel where

import Codec.Picture

data RayTraceModel = RayTraceModel {
    modelNumSamples :: Int,
    modelImage :: Image PixelRGB8
} 
