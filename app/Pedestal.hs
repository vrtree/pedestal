{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens.Extra
import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Graphics.GL.Freetype
import TinyRick

import Halive.Utils

import Types
import Shapes


{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [VRPalDevices]
enableDevices = [UseOpenVR]
-- enableDevices = []

buildSculptures :: Font -> IO (Map SculptureID Sculpture)
buildSculptures font = do
  sculptureGeo <- cubeGeometry (V3 sculptureSize sculptureSize sculptureSize) (V3 1 1 1)

  let sculptureShaders = [ "default"
                         -- , "pit"
                         -- , "noiseStep"
                         -- , "weirdHoles1"
                         -- , "fieldSub"
                         -- , "bubbles"
                         -- , "cubeSubField"
                         -- , "tessel"
                         -- , "tesselSphere"
                         ]
  fmap Map.fromList $ forM (zip [0..] sculptureShaders) $ \(i, shaderName) -> do
    let shaderPath = "app/shaders/sculptures/" ++ shaderName ++ ".frag"
    shaderComp <- shaderRecompiler "app/shaders/raytrace.vert" shaderPath (makeShape sculptureGeo)
    
    textRenderer <- textRendererFromFile font shaderPath
    let sculpture = Sculpture
                  { _scpPose         = newPose { _posPosition = V3 0 0 0}
                  , _scpGetShape     = shaderComp 
                  , _scpTextRenderer = textRenderer
                  , _scpScroll       = 0
                  }

    return (i, sculpture)

newWorld :: Font -> IO World
newWorld font = do
  sculptures <- buildSculptures font
  return World 
        { _wldSculptures = sculptures
        , _wldFocusedSculptureID = 0
        , _wldObjects =  flip map [0..9] $ 
                            \x -> newPose { 
                              _posPosition = V3 (0.3 * sin x) 
                                                (0.3 * cos x + 1) 
                                                (0.3 * sin (4 * cos x))
                              }  
        , _wldPlayer  = newPose { _posOrientation = axisAngle (V3 0 1 0) 3.14 }
        -- Moves world left and forward for seated dev
        -- , _wldPlayer  = newPose { _posPosition = V3 (-2) 0 1 }
        , _wldRoom    = Room { _romPose = newPose }
        , _wldTime    = 0
        , _wldLight   = newPose { _posPosition = V3 1 2 0 }
        }

main :: IO ()
main = do

  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Pedestal" enableDevices
  
  glyphProg <- createShaderProgram "app/shaders/glyph.vert" "app/shaders/glyph.frag"
  font      <- createFont "app/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

  {-

    Setting up some GL information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  shapes <- galleryShapes

  {-

    Building our default world.

  -}

  world <- reacquire 1 (newWorld font)
  -- world <- newWorld font

  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do
    
    persistState 1

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    -- applyMouseLook gpWindow wldPlayer
    -- applyWASD gpWindow wldPlayer

    focusedSculptureID <- use wldFocusedSculptureID 
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      -- applyGamepadJoystickMovement e wldPlayer

      -- Scroll the active rick
      onScroll e $ \_ scrollY -> do
        wldSculptures . ix focusedSculptureID . scpScroll %= \s ->
          min 100 (max (-1000) (s + scrollY))

      -- Pass events to the active sculpture
      handleTextBufferEvent gpWindow e (wldSculptures . ix focusedSculptureID . scpTextRenderer)

    view44 <- viewMatrixFromPose <$> use wldPlayer

    -- Once we have set up all the neccesary information,
    -- Render away!
    immutably $ renderWith vrPal view44 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes)





{-

  Render function.

  This should be totally pure! only take in world state, and feed back
  delicious pixels on the screen

-}



render :: (MonadIO m, MonadReader World m) 
       => Shapes Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection44 view44 = do

  let roomShape          = shapes ^. shpRoom
      pedestalShape      = shapes ^. shpPedestal
      lightShape         = shapes ^. shpLight
      codeHolderShape    = shapes ^. shpCodeHolder

  objects <- view wldObjects

  {-

    Render the Room

  -}

  glEnable GL_CULL_FACE
  glCullFace GL_FRONT

  room <- view wldRoom

  useProgram (sProgram roomShape)

  withVAO (sVAO roomShape) $ do

    let model44 = transformationFromPose $ shiftBy roomOffset (room ^. romPose)  
    drawShape' model44 projection44 view44 roomShape


  {-

    Render the Light

  -}

  glCullFace GL_BACK

  light <- view wldLight

  useProgram (sProgram lightShape)

  withVAO (sVAO lightShape) $ do

    let model44 = transformationFromPose light 
    drawShape' model44 projection44 view44 lightShape

    forM_ objects $ \p -> do

      let model44' = transformationFromPose p
      drawShape' model44' projection44 view44 lightShape



  {-

    Render the Pedestals

  -}
  sculptures <- view wldSculptures

  useProgram (sProgram pedestalShape)

  withVAO (sVAO pedestalShape) $ do

    forM_ sculptures $ \obj -> do

      let model44 = transformationFromPose $ shiftBy pedestalOffset (obj ^. scpPose)  
      drawShape' model44 projection44 view44 pedestalShape


  {-

    Render the CodeHolders

  -}

  useProgram (sProgram codeHolderShape)

  withVAO (sVAO codeHolderShape) $ do

    forM_ sculptures $ \obj -> do

      let pose = Pose { _posPosition = (obj ^. scpPose . posPosition)  
                      , _posOrientation = axisAngle (V3 1 0 0) 0.4
                      }
          model44 = transformationFromPose $ shiftBy (V3 0 (-0.25) (0.4)) pose
      drawShape' model44 projection44 view44 codeHolderShape
  -- And their code
  -- glDisable GL_DEPTH_TEST
  glDisable GL_CULL_FACE
  glEnable  GL_BLEND
  forM_ sculptures $ \obj -> do
    let pose   = id
               . rotateBy (axisAngle (V3 1 0 0) (-pi/4 - 0.4))
               . shiftBy (V3 (-0.1) (0.57) 0.55)
               $ newPose { _posPosition = obj ^. scpPose . posPosition }
        model44 = transformationFromPose pose !*! scaleMatrix 0.3
        mvp = projection44 !*! view44 !*! model44
    renderText (obj ^. scpTextRenderer) mvp (V3 1 1 1)
  -- glEnable GL_DEPTH_TEST
  glEnable  GL_CULL_FACE
  glDisable GL_BLEND
  {-

    Render the Sculptures

    Draw them last because they are going to have their backsides shown

  -}

  -- FIXME this needs to be synced with raytrace.frag NUM_POINTS
  let points = take 4 $ flip map objects $ \i -> (i ^. posPosition)

  --liftIO $ putStrLn $ show (points !! 0)
  --liftIO $ putStrLn $ show (points !! 1)

  glDisable GL_CULL_FACE
  
  forM_ sculptures $ \obj -> do

    (shape, anyError) <- liftIO $ obj ^. scpGetShape
    useProgram (sProgram shape)

    let Uniforms{..} = sUniforms shape

    uniformV3 uDimensions (V3 (sculptureSize) (sculptureSize) (sculptureSize))

    uniformV3V uPoints points

    withVAO (sVAO shape) $ do

      let model44 = transformationFromPose $ shiftBy sculptureOffset (obj ^. scpPose)  
      drawShape' model44 projection44 view44 shape






{-

  Helper functions for drawing

-}

drawShape' ::(MonadIO m, MonadReader World m) => M44 GLfloat -> M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape' model44 projection44 view44 shape = do 

  let Uniforms{..} = sUniforms shape

  light <- view wldLight
  time  <- view wldTime

  -- Recalculating for each object. doesn't make sense!
  uniformV3 uEye (inv44 view44 ^. translation)
  uniformV3 uLight (light ^. posPosition - V3 0 0.3 0)
  uniformF  uTime time

  uniformM44 uViewProjection      (projection44 !*! view44)
  uniformM44 uModelViewProjection (projection44 !*! view44 !*! model44)
  uniformM44 uInverseModel        (inv44 model44)
  uniformM44 uModel               model44
  uniformM44 uNormalMatrix        (transpose . inv44 $ view44 !*! model44 )

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr






 
