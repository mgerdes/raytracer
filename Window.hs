module Window where

import Debug.Trace
import Codec.Picture
import Control.Monad (unless)
import System.Exit
import System.IO
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as GL

import RayTraceModel

loadGLTextureFromFile :: FilePath -> IO GL.TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Linear')
                             texture2DWrap $= (Mirrored, ClampToEdge)
                             return t

loadGLTexture :: Image PixelRGB8 -> IO GL.TextureObject
loadGLTexture (Image w h p) = do t <- loadTexture (texInfo w h TexRGB p)
                                 textureFilter Texture2D $= ((Linear', Nothing), Linear')
                                 texture2DWrap $= (Mirrored, ClampToEdge)
                                 return t

drawImage :: (RayTraceModel -> RayTraceModel) -> RayTraceModel -> IO ()
drawImage updateModel model = do
    let errorCallback err description = hPutStrLn stderr description
    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    if not successfulInit
        then exitFailure
        else do
          mw <- G.createWindow 800 400 "Simple example, haskell style" Nothing Nothing
          case mw of Nothing -> (G.terminate >> exitFailure)
                     Just window -> do
                                    G.makeContextCurrent mw
                                    preMainLoop window updateModel model
                                    G.destroyWindow window
                                    G.terminate
                                    exitSuccess



preMainLoop :: G.Window -> (RayTraceModel -> RayTraceModel) -> RayTraceModel -> IO ()
preMainLoop window updateModel model = do
    clearColor $= Color4 0.9 0.1243 0.2544564 1.0
    depthFunc $= Just Lequal
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    normalize $= Enabled
    texture Texture2D $= Enabled
    shadeModel $= Smooth
    mainLoop window updateModel model

mainLoop :: G.Window -> (RayTraceModel -> RayTraceModel) -> RayTraceModel -> IO ()
mainLoop window updateModel model = do
    tex <- loadGLTexture (modelImage model)
    action <- (G.windowShouldClose window)
    unless action $ do
        viewWindow window
        cal tex
        G.swapBuffers window
        G.pollEvents
        mainLoop window updateModel (updateModel model)

cal tex = do
    preservingMatrix $ do
        rotate 90 (Vector3 1 0 0 :: Vector3 GLfloat)
        withTextures2D [tex] $ draw tex

draw tex = do
    textureBinding Texture2D $= Just tex
    renderPrimitive Quads $ do
    n 0 1 0
    t 1 1 >> v   2  (-1)   1
    t 1 0 >> v   2  (-1) (-1)
    t 0 0 >> v (-2) (-1) (-1)
    t 0 1 >> v (-2) (-1)   1
    where v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
          n x y z = normal (Normal3 x y z :: Normal3 GLfloat)
          t u v   = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)

viewWindow window = do
    (width, height) <- G.getFramebufferSize window
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    ortho (negate ((fromIntegral width) / (fromIntegral height))) ((fromIntegral width) / (fromIntegral height)) (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0
