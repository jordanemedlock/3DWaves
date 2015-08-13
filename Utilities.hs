-- |
-- Module      : Southpaw.WaveFront.Utilities
-- Description : Parsing utilities
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 15 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.WaveFront.Utilities where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Text.Read (readEither)


---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Parsing utilities ------------------------------------------------------------------------------
-- | This strikes me as overly convoluted. Should probably be refactored or removed entirely.
parseTokenWith :: String -> (String -> Either e token) -> Int -> (Int, Either e token, String)
parseTokenWith line parse = withoutComment line parse


-- | Passes the input line (stripped of any trailing comments) to the specified parser
withoutComment :: String -> (String -> a) -> Int -> (Int, a, String)
withoutComment (l:ine) parse lnum = let (l:tokens, comment) = span (/= '#') ine in (lnum, parse tokens, comment)


-- | Predicate for determining if a single line of text is a comment.
--   Comments are preceded by a '#' and any number of whitespace characters.
--
-- TODO: Drop comments at the end of a line (?)
-- TODO: Add stripComment (or extractComment) which consumes a line up until the first '#'.
-- This would allow for tokens and comments to appear on the same line.
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- | Strips a trailing comment from an MTL or OBJ line.
dropComment :: String -> String
dropComment = takeWhile (/= '#')


-- |
takeComment :: String -> String
takeComment = dropWhile (/= '#')


-- |
enumerate :: [(Int -> (Int, token, comment))] -> [(Int, token, comment)]
enumerate = zipWith (flip ($)) [1..]


-- | Splits a string into rows and filters out unimportant elements (empty lines and comments)
-- NOTE: This function is probably obsolete due to comments being included by the parsers
-- TODO: Higher order function for composing predicates
rows :: String -> [String]
rows = filter (not . satisfiesAny [null, isComment]) . lines
  where satisfiesAny predicates x = any ($ x) predicates


-- |
-- TODO: Use readMaybe (?)
-- TODO: Variadic 'unpacking' (or is that sinful?)
-- TODO: More informative error message (?)
-- TODO: Rename (?)
-- TODO: Generic function for mapping and sequencing readEither (cf. "vt" case in parseOBJRow) (?)
vector :: Read r => ([r] -> b) -> [String] -> e -> Either e b
vector f coords e = either (Left . const e) (Right) (sequence (map readEither coords) >>= Right . f)
-- vector _ _      = Left  $ "Wrong number of coordinates for vector"


-- |
second :: (a, b, c) -> b
second (_, b, _) = b

-- |
third :: (a, b, c) -> c
third (_, _, c) = c
