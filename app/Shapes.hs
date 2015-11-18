{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Shapes where

import Graphics.GL.Pal

import Types

galleryShapes :: IO (Shapes Uniforms)
galleryShapes = do 

  {-

    Setting up resources that will be used across multiple 
    objects

  -}
  roomProg         <- createShaderProgram "app/shaders/world/room.vert" "app/shaders/world/room.frag"
  roomGeo          <- cubeGeometry (V3 roomWidth roomHeight roomDepth) (V3 1 1 1)
  roomShape        <- makeShape roomGeo roomProg

  pedestalProg     <- createShaderProgram "app/shaders/world/pedestal.vert" "app/shaders/world/pedestal.frag"
  pedestalGeo      <- cubeGeometry ((V3 sculptureSize pedestalHeight sculptureSize)) (V3 1 1 1)
  pedestalShape    <- makeShape pedestalGeo pedestalProg

  codeHolderProg   <- createShaderProgram "app/shaders/world/pedestal.vert" "app/shaders/world/pedestal.frag"
  codeHolderGeo    <- cubeGeometry ((V3 (sculptureSize - 0.1) (3 * pedestalHeight) (sculptureSize- 0.1))) (V3 1 1 1)
  codeHolderShape  <- makeShape codeHolderGeo codeHolderProg

  lightProg        <- createShaderProgram "app/shaders/world/light.vert" "app/shaders/world/light.frag"
  lightGeo         <- icosahedronGeometry 0.02 4
  lightShape       <- makeShape lightGeo lightProg


  let shapes = Shapes { _shpRoom        = roomShape
                      , _shpLight       = lightShape
                      , _shpPedestal    = pedestalShape
                      , _shpCodeHolder  = codeHolderShape
                      }

  return shapes


