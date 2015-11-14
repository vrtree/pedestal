{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Shapes where

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

galleryShapes :: IO (Shapes Uniforms)
galleryShapes = do 

  {-

    Setting up resources that will be used across multiple 
    objects

  -}
  roomProg       <- createShaderProgram "app/shaders/world/room.vert" "app/shaders/world/room.frag"
  roomGeo        <- cubeGeometry (V3 roomWidth roomHeight roomDepth) (V3 30 30 30)
  roomShape      <- makeShape roomGeo roomProg

  pedestalProg   <- createShaderProgram "app/shaders/world/pedestal.vert" "app/shaders/world/pedestal.frag"
  pedestalGeo    <- cubeGeometry ((V3 sculptureSize pedestalHeight sculptureSize)) (V3 20 30 20)
  pedestalShape  <- makeShape pedestalGeo pedestalProg

  lightProg   <- createShaderProgram "app/shaders/world/light.vert" "app/shaders/world/light.frag"
  lightGeo    <- icosahedronGeometry 0.1 4
  lightShape  <- makeShape lightGeo lightProg

  sculptureGeo   <- cubeGeometry ((V3 sculptureSize sculptureSize sculptureSize)) (V3 1 1 1)

  let vs = "app/shaders/raytrace.vert"


  sDefault          <- createShaderProgram vs "app/shaders/sculptures/raytrace.frag"

  sPit              <- createShaderProgram vs "app/shaders/sculptures/pit.frag"
  sNoiseStep        <- createShaderProgram vs "app/shaders/sculptures/noiseStep.frag"
  sWeirdHoles1      <- createShaderProgram vs "app/shaders/sculptures/weirdHoles1.frag"
  sFieldSub         <- createShaderProgram vs "app/shaders/sculptures/fieldSub.frag"
  sBubbles          <- createShaderProgram vs "app/shaders/sculptures/bubbles.frag"
  sCubeSubField     <- createShaderProgram vs "app/shaders/sculptures/cubeSubField.frag"
  sTessel           <- createShaderProgram vs "app/shaders/sculptures/tessel.frag"
  sTesselSphere     <- createShaderProgram vs "app/shaders/sculptures/tesselSphere.frag"


  s1  <- makeShape sculptureGeo sDefault

  let shapes = Shapes{ _shpRoom        = roomShape
                     , _shpLight       = lightShape
                     , _shpPedestal    = pedestalShape
                     , _shpSculptures  = [s1]
                     }

  return shapes
