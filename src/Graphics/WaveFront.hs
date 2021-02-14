
--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO | - Decide on an API
module Graphics.WaveFront (
  -- * types
  module Types,
  
  -- * Lenses
  -- module Lenses,
  
  -- * Parsing
  -- module Graphics.WaveFront.Parse,
  
  -- * Model functions
  -- createModel, tessellate, bounds, fromIndices, fromFaceIndices, diffuseColours, hasTextures, textures,

  -- * Loading
  model,
  

) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Graphics.WaveFront.Types as Types
-- import Graphics.WaveFront.Parse
import Graphics.WaveFront.Parse.Common
import Graphics.WaveFront.Model
import Graphics.WaveFront.Load ( obj, mtl, materials, model )
