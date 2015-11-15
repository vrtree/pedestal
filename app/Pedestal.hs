{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens
import Data.Data
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Fixed
import Debug.Trace
import System.Random
import Control.Monad.Random

import Halive.Utils

import Types
import Shapes


{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [VRPalDevices]
-- enableDevices = [UseOpenVR]
-- enableDevices = [UseOpenVR, UseHydra]
enableDevices = []

main :: IO ()
main = do

  gamePal@VRPal{..} <- reacquire 0 $ initVRPal "Gallery" NoGCPerFrame enableDevices


  {-

    Setting up some gl information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  shapes <- galleryShapes

  {-

    Building our default world.

    Because of the "!" s in the type declaration
    we need to declare all parts of the world, 
    or it be break in right away

  -}

  world <- reacquire 1 $ return World 
        { _wldSculptures = Map.fromList $ flip map [0] $ 
                          \i -> let something = Sculpture
                                      { _scpPose  = newPose { _posPosition = V3 0 0 0} 
                                      }
                                in (i, something)
        , _wldObjects =  flip map [0..9] $ 
                            \x -> newPose { _posPosition = V3 (0.3 * (sin x)) ((0.3 * (cos x))+1) (0.3  * (sin (4 * cos x))) }  
        , _wldPlayer  = Pose {_posOrientation = axisAngle (V3 0 1 0) 3.14 , _posPosition = V3 0 0 0}
        , _wldRoom    = Room { _romPose = newPose {_posPosition = V3 0 0 0} }
        , _wldTime    = 0
        , _wldLight   = newPose {_posPosition = V3 1 2 0}
        }


  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do
    
    persistState 1

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    time <- use wldTime

    objects <- use wldObjects


    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer


    -- controls for debugging 
    shiftDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'LeftShift
    whenKeyPressed gpWindow Key'Z           $ liftIO $ putStrLn $ "oh" ++ show [((objects !! 0) ^. posPosition )] ++ " yeah"

    viewMat <- viewMatrixFromPose <$> use wldPlayer


    -- Once we have set up all the neccesary information,
    -- Render away!
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes)





{-

  Render function.

  This should be totally pure! only take in world state, and feed back
  delicious pixels on the screen

-}



render :: (MonadIO m, MonadState World m) 
       => Shapes Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection viewMat = do

  time <- use wldTime


  let roomShape          = shapes ^. shpRoom
  let pedestalShape      = shapes ^. shpPedestal
  let lightShape         = shapes ^. shpLight
  let codeHolderShape    = shapes ^. shpCodeHolder

  let sculptureShapes    = shapes ^. shpSculptures

  objects <- use wldObjects

  {-

    Render the Room

  -}

  glEnable GL_CULL_FACE
  glCullFace GL_FRONT

  room <- use wldRoom

  useProgram (sProgram roomShape)

  withVAO (sVAO roomShape) $ do


    let model = transformationFromPose $ shiftBy roomOffset (room ^. romPose)  
    drawShape' model projection viewMat roomShape


  {-

    Render the Light

  -}

  glCullFace GL_BACK

  light <- use wldLight

  useProgram (sProgram lightShape)

  withVAO (sVAO lightShape) $ do

    let model = transformationFromPose light 
    drawShape' model projection viewMat lightShape

    forM_ ( zip [0..] ( objects ) ) $ \( i , p ) -> do

      let model = transformationFromPose p
      drawShape' model projection viewMat lightShape



  {-

    Render the Pedestals

  -}
  sculptures <- use wldSculptures

  useProgram (sProgram pedestalShape)

  withVAO (sVAO pedestalShape) $ do

    forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

      let model = transformationFromPose $ shiftBy pedestalOffset  (obj ^. scpPose)  
      drawShape' model projection viewMat pedestalShape


  {-

    Render the CodeHolders

  -}
  sculptures <- use wldSculptures

  useProgram (sProgram codeHolderShape)

  withVAO (sVAO codeHolderShape) $ do

    forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

      let pose = Pose { _posPosition = (obj ^. scpPose . posPosition)  
                      , _posOrientation = axisAngle (V3 1 0 0 ) 0.4
                      }
          model = transformationFromPose $ shiftBy (V3 0 (-0.25) (0.4) ) pose
      drawShape' model projection viewMat codeHolderShape



  {-

    Render the Sculptures

    Draw them last because they are going to have their backsides shown

  -}

  let points = flip map objects $ \i -> (i ^. posPosition)

  --liftIO $ putStrLn $ show (points !! 0)
  --liftIO $ putStrLn $ show (points !! 1)

  glDisable GL_CULL_FACE
  
  sculptures <- use wldSculptures

  forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

    let shape = (sculptureShapes !! i)
    useProgram (sProgram shape)

    let Uniforms{..} = sUniforms shape

    uniformV3 uDimensions (V3 (sculptureSize) (sculptureSize) (sculptureSize))

    uniformV3V uPoints points

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy sculptureOffset (obj ^. scpPose)  
      drawShape' model projection viewMat shape






{-

  Helper functions for drawing


-}

drawShape' ::(MonadIO m, MonadState World m) => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape' model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  light <- use wldLight
  time  <- use wldTime


  -- Recalculating for each object. doesn't make sense!
  uniformV3 uEye (fromMaybe view (inv44 view) ^. translation)
  uniformV3 uLight ((light ^. posPosition)- (V3 0 0.3 0))
  uniformF  uTime time

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr






 
