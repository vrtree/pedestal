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
import Data.Maybe
import qualified Data.Map.Strict as Map

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

  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Gallery" NoGCPerFrame enableDevices


  {-

    Setting up some GL information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  shapes <- galleryShapes

  {-

    Building our default world.

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

    objects <- use wldObjects


    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer


    -- controls for debugging 
    whenKeyPressed gpWindow Key'Z $ liftIO $ putStrLn $ "oh" ++ show [((objects !! 0) ^. posPosition )] ++ " yeah"

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
      sculptureShapes    = shapes ^. shpSculptures

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

      let model44 = transformationFromPose $ shiftBy pedestalOffset  (obj ^. scpPose)  
      drawShape' model44 projection44 view44 pedestalShape


  {-

    Render the CodeHolders

  -}

  useProgram (sProgram codeHolderShape)

  withVAO (sVAO codeHolderShape) $ do

    forM_ sculptures $ \obj -> do

      let pose = Pose { _posPosition = (obj ^. scpPose . posPosition)  
                      , _posOrientation = axisAngle (V3 1 0 0 ) 0.4
                      }
          model44 = transformationFromPose $ shiftBy (V3 0 (-0.25) (0.4) ) pose
      drawShape' model44 projection44 view44 codeHolderShape



  {-

    Render the Sculptures

    Draw them last because they are going to have their backsides shown

  -}

  let points = flip map objects $ \i -> (i ^. posPosition)

  --liftIO $ putStrLn $ show (points !! 0)
  --liftIO $ putStrLn $ show (points !! 1)

  glDisable GL_CULL_FACE
  
  forM_ (zip [0..] (Map.elems sculptures)) $ \(i , obj) -> do

    let shape = (sculptureShapes !! i)
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
  uniformV3 uEye (fromMaybe view44 (inv44 view44) ^. translation)
  uniformV3 uLight (light ^. posPosition - V3 0 0.3 0)
  uniformF  uTime time

  uniformM44 uViewProjection      (projection44 !*! view44)
  uniformM44 uModelViewProjection (projection44 !*! view44 !*! model44)
  uniformM44 uInverseModel        (fromMaybe model44 (inv44 model44))
  uniformM44 uModel               model44
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view44 !*! model44 )

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr






 
