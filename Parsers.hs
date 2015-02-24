--
-- Wavefront - Parsers.hs
-- Parser and loader for WaveFront obj models
--
-- Jonatan H Sundqvist
-- February 8 2015
-- 
-- Migrated to separate project on February 21 2015

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--        - Separate MTL and OBJ parsers (?) (...)
--        - Separate parsing, IO and testing (...)
--        - Additional attributes (lighting, splines, etc.)
--        - FFI (...)
--        - Debugging information (line number, missing file, missing values, etc.) (...)
--        - Proper Haddock coverage, including headers (...)
--        - Model type (...)
--        - Caching (?)
--        - Performance, profiling, optimisations
--        - PrintfArg instances for the types defined in this module
--        - Decide on a public interface (exports)
--        - Reconciling Cabal and hierarchical modules

-- SPEC | -
--        -


{-# LANGUAGE ForeignFunctionInterface #-}

module Parsers (parseOBJ, parseMTL, loadOBJ, loadMTL) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf, unfoldr)
import Data.Char (isSpace)
import Data.Either (rights)
import qualified Data.Map as M



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- | Represents a single (valid) OBJ token
--
-- TODO: Polymorphic numerical types (?)
-- TODO: Add context, metadata (eg. line numbers, filename) (?)
--
data OBJToken = Vertex  Float Float Float |
                Normal  Float Float Float |
                Texture Float Float       |
                Face [(Int, Int, Int)]    | -- TODO: Associate material with each face, handle absent indices

                UseMTL String | --  
                LibMTL String | -- TODO: Use actual MTL type

                Group  [String] | -- TODO: Do grouped faces have to be consecutive?
                Object [String]   -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
--
-- TODO: Use error type instead of String (?)
-- This would allow us to distinguish invalid data from eg. comments and blank lines
--
type OBJRow = Either String OBJToken


-- | Output type of the OBJ parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
--
type OBJ = [(Int, OBJRow)]


-- | Abstract representation of an OBJ model with associated MTL definitions.
-- 
--
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
-- TODO: Reconsider the types (especially of the materials)
--
data Model = Model { vertices  :: [OBJToken],
                     normals   :: [OBJToken],
                     textures  :: [OBJToken],
                     faces     :: [OBJToken],
                     selects   :: [OBJToken],              -- TODO: Rename (UseMTL) (?) 
                     materials :: M.Map String [MTLToken], -- TODO: Type synonym (?)
                     groups    :: [OBJToken],
                     objects   :: [OBJToken] } deriving (Show)


-- | Represents a single (valid) MTL token
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- TODO: Include support for ('Ns', 'Ni', 'd', 'Tr', 'illum')
-- 
data MTLToken = Ambient  Float Float Float | -- Ka
                Diffuse  Float Float Float | -- Kd
                Specular Float Float Float | -- Ks

        MapDiffuse String | -- map_Kd
        Material   String   -- newmtl
        deriving (Eq, Show)


-- | Output type of the single-row MTL parser. 
type MTLRow = Either String MTLToken


-- | Output type of the MTL parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Add type for processed MTL (eg. a map between names and materials)
--
type MTL = [(Int, MTLRow)]



---------------------------------------------------------------------------------------------------
-- Functions (pure)
---------------------------------------------------------------------------------------------------
-- Parsers ----------------------------------------------------------------------------------------
-- | This function creates an OBJToken or error for each line in the input data
-- 
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Is it even necessary to strip whitespace?
-- TODO: Function for composing predicates (?)
-- TODO: Should this function separate the various fields (eg. [(Vertices, Faces, Materials, Groups)] instead of [Maybe OBJToken])
--
parseOBJ :: String -> OBJ
parseOBJ = zip [1..] . map parseOBJRow . lines -- . rows

-- I never knew pattern matching in list comprehensions could be used to filter by constructor
-- let rows = parseOBJ data in ([ v | @v(Vertex {}) <- rows], [ v | @v(Vertex {}) <- rows])


-- | Generates a token given a single
--
-- TODO: Correctness (complete function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, currently unsupported attributes (ignore?) (pattern match against the entire line, eg. ["vn", x, y, z])
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indices
--
parseOBJRow :: String -> OBJRow -- Maybe OBJToken
parseOBJRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = let (which:values) = words ln in case which of
    "v"  -> vector Vertex values -- Vertex
    "vn" -> vector Normal values -- Normal
    -- TODO: Clean this up
    -- TODO: More generic way of unpacking the right number of values and applying read (?)
    "vt" -> let (x:y:[]) = values in Right $ Texture (read x) (read y) -- Texture
    -- TODO: Clean this up
    -- TODO: Handle invalid data (✓)
    -- TODO: Capture invalid vertex definitions (cf. sequence) (✓)
    -- ("Invalid vertex: "++) .
    "f"  -> either (Left . const ln) (Right . Face) . sequence . map (vector triplet . splitOn '/') $ values -- Face
    "g"  -> Right . Group  $ values -- Group
    "o"  -> Right . Object $ values -- Object
    "s"  -> Left ln -- Smooth shading
    "mtllib" -> Right . LibMTL $ head values --
    "usemtl" -> Right . UseMTL $ head values --
    _        -> Left ln
    where triplet a b c = (a, b, c) -- TODO: Use tuple sections (?)


-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = zip [1..] . map parseMTLRow . lines


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> MTLRow
parseMTLRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = let (which:values) = words ln in case which of
    "Ka" -> withChannels Ambient  values -- Ka
    "Kd" -> withChannels Diffuse  values -- Kd
    "Ks" -> withChannels Specular values -- Ks
    "map_Kd" -> withName MapDiffuse values -- map_Kd
    "newmtl" -> withName Material   values -- newmtl
    _        -> Left ln
    where withChannels token (r:g:b:[]) = Right $ token (read r) (read g) (read b) -- TODO: No alpha channel (optional?) (?) (read a) 
          withChannels _      _         = Left  $ "Pattern match failed"           -- TODO: No alpha channel (optional?) (?) (read a)

          withName token (name:[]) = Right $ token name
          withName _      _        = Left "Pattern match failed"


-- | 
-- TODO: Use map for materials (?)
-- TODO: How to retrieve MTL data
-- TODO: How to deal with errors, including no-parse, index errors, etc.
-- TODO: Performance, how are 'copies' of coordinates handled (?)
-- TODO: Use a more efficient data structure (especially w.r.t indexing; cf. Vector)
-- TODO: Consider preserving the indices (rather than generating a list of duplicated vertices).
--       This would preserve space (in cases where vertices are often re-used), as well as being
--       very compatible with index arrays on graphics cards.
--
createModel :: OBJ -> ([String] -> M.Map String [MTLToken]) -> Model
createModel modeldata retrieve = let tokens       = rights . map snd $ modeldata -- TODO: Vat do vee du viz ze dissidents, kommandant?
                                     theMaterials = retrieve [ name | UseMTL name   <- tokens ] -- Retrieve MTL data
                                     theVertices  = [ vertex  | vertex@(Vertex{})   <- tokens ]
                                     theNormals   = [ normal  | normal@(Normal{})   <- tokens ]
                                     theTextures  = [ texture | texture@(Texture{}) <- tokens ]
                                     theFaces     = [ face    | face@(Face{})       <- tokens ]
                                     theGroups    = [ group   | group@(Face{})      <- tokens ]
                                     theObjects   = [ object  | object@(Face{})     <- tokens ]
                                     theSelects   = [ select  | select@(Face{})     <- tokens ]
                                     theObject    = [ object  | object@(Face{})     <- tokens ]
                                 in Model { vertices  = theVertices,
                                            normals   = theNormals,
                                            textures  = theTextures,
                                            faces     = theFaces,
                                            selects   = theSelects, -- TODO: Rename (UseMTL) (?) 
                                            groups    = theGroups,
                                            objects   = theObjects,
                                            materials = theMaterials } 


-- Parsing utilities ------------------------------------------------------------------------------
-- |
-- TODO: Clean up or use existing function
-- TODO: Rename (?)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = unfoldr cut s
  where cut [] = Nothing
        cut xs = let (token, rest) = span (/=c) xs in Just (token, dropWhile (==c) rest)


-- | Predicate for determining if a String is a comment. Comments are preceded by a '#' and any
-- number of whitespace characters (not including linebreaks). Support for comments at the end
-- of a line has yet to be added.
--
-- TODO: Drop comments at the end of a line (?)
-- TODO: Add stripComment (or extractComment) which consumes a line up until the first '#'.
-- This would allow for tokens and comments to appear on the same line.
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- | Splits a string into rows and filters out unimportant elements (empty lines and comments)
-- NOTE: This function is probably obsolete due to comments being included by the parsers
-- TODO: Higher order function for composing predicates
rows :: String -> [String]
rows = filter (\ ln -> not $ any ($ ln) [null, isComment]) . lines


-- |
-- TODO: Use readMaybe (?)
-- TODO: Variadic 'unpacking' (or is that sinful?)
vector :: Read r => (r -> r -> r -> b) -> [String] -> Either String b
vector token (x:y:z:[]) = Right $ token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
vector _      _         = Left  "Pattern match failed"



---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------
-- Loading data -----------------------------------------------------------------------------------
-- |
-- TODO: Use bytestrings (?)
loadOBJ :: String -> IO OBJ
loadOBJ fn = do
  rawOBJ <- readFile fn    --
  return $ parseOBJ rawOBJ --


-- |
-- TODO: Use bytestrings (?)
loadMTL :: String -> IO MTL
loadMTL fn = do
  rawMTL <- readFile fn    --
  return $ parseMTL rawMTL --


-- |
-- Loads an OBJ model from file, including associated materials
loadModel :: String -> IO Model
loadModel fn = do
  obj <- loadOBJ fn
  return $ error "Not done yet"



---------------------------------------------------------------------------------------------------
-- Pure foreign function interface
---------------------------------------------------------------------------------------------------
-- foreign export ccall parseOBJ :: String -> OBJ
-- foreign export ccall parseMTL :: String -> MTL