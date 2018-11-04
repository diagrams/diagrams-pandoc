-- {-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
module Diagrams.Pandoc where

import           Control.Applicative
import           Control.Lens                hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char                   (toLower)
import           Data.Data.Lens
import           Data.Foldable               (foldMap)
import           Data.List                   (delete)
import           Data.Maybe
import           Diagrams.Backend
import           Diagrams.Builder            as DB
import           Diagrams.Prelude            hiding (block)
import           System.FilePath
import           System.IO
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON
import           Text.Read

import Diagrams.Builder.Opts

import           Diagrams.Backend.PGF
import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.SVG

-- | Default size to use when nothing else is given.
defaultSize :: Num n => SizeSpec V2 n
defaultSize = dims2D 260 120

-- | General walk over the blocks in a Pandoc document with access to
--   the Meta and Format.
--
--   A common way to use this is with Pandoc's JSON filter:
--
-- @
-- toJSONFilter $ pandocFilter (backendFilter defaultFilters)
-- @
pandocFilter :: Monad m => (Meta -> Maybe Format -> Block -> m [Block])
             -> Maybe Format -> Pandoc -> m Pandoc
pandocFilter f mf (Pandoc m bs) = Pandoc m `liftM` walkM' (f m mf) bs
  where
    -- (Block -> m [Block]) -> [Block] -> m [Block]
    walkM' g = bottomUpM (liftM concat . mapM g)

-- | An ad-hoc filter to build a diagram from a block. Putting them in a
-- hetrogeneous container like this allows multiple backends to be used
-- in a single document.
data BackendFilter = forall b. BackendBuild b => BackendFilter
  { nameMatch   :: String -> Bool  -- matches name of backend
  , formatMatch :: Format -> Bool  -- matches format
  , defaultOpts :: Options b       -- default options to use
  , filterBuild :: Maybe Format -> DiaBuildOpts -> Attr -> String -> IO Block
                                   -- Function to build diagram
  }

type OptsAdjust = DiaBuildOpts -> DiaBuildOpts

-- | Filters for 'Rasterific', 'SVG' and 'PGF' backends. Any other
-- modules included by CPP will also be in this list. Rasterific is the
-- first in the list so it will be used as fallback if no backend or
-- 'Format' is found.
-- defaultFilters :: [BackendFilter]
-- defaultFilters = [rasterificFilter, svgFilter, pgfFilter]

-- | Filter for turning 'CodeBlock's into diagrams using a list of
--   'BackendFilter's. If no filters are given the pandoc output will
--   show a message saying so. A message will also been shown in the
--   document if there is an error in interpreting or compiling the
--   diagram.
--
--   Currently implemented as follows:
--
--   * Diagrams are specified with the @.diagram@ class. There are also
--     @.code-diagram@ and @.diagram-code@ classes for including code
--     before/after respectively.
--
--     @@
--     ``` diagram-code
--     diagram = circle 3
--     ```
--     @@
--
--   * Backends are specified by the @default-backend@ 'Meta' value or
--     the @backend@ key of the code block (case insensitive).
--
--     @@
--     ---
--     title: My SVG diagrams
--     default-backend: svg
--     ...
--     @@
--     @@
--     ``` {.diagram backend=pgf}
--     diagram = square 3 # fc blue # lw thick
--     ```
--     @@
--     @@
--     ``` {.diagram backend=Rasterific width=300}
--     diagram = triangle 2 # fc yellow
--     @@
backendFilter
  :: MonadIO m
  => OptsAdjust
  -> [BackendFilter]
  -> Meta
  -> Maybe Format
  -> Block
  -> m [Block]
-- backendFilter optsAdjust filters meta@(Meta m) mf (CodeBlock attrs@(bId, classes, keys) code)
--   | "diagram"      `elem` classes = mkDiagram
--   | "diagram-code" `elem` classes = (++ codeBlock) `liftM` mkDiagram
--   | "code-diagram" `elem` classes = (codeBlock ++) `liftM` mkDiagram
--   where
--     mkDiagram = liftIO $ case backend of
--       Just (BackendFilter _ _ opts f) ->
--         let bOpts = mkBuildOpts undefined undefined opts
--                       & keysOptsAlter meta attrs
--                       & optsAdjust
--         in  pure `liftM` f mf bOpts attrs code
--       Nothing -> return [code_ "A diagram should be here but no backend filters where found."]

--     codeBlock = [CodeBlock (bId, rmCode classes, keys) code]
--     rmCode    = cons "haskell" . delete "diagram-code" . delete "code-diagram"
--     backend   = test nameMatch bName <|> test formatMatch mf <|> listToMaybe filters
--     -- try to match names or formats, use head as fallback
--     bName     = map toLower
--             <$> lookup "backend" keys
--             <|> m ^? foldMap ix ["default-backend", "diagrams-backend", "backend"] . template
--     -- query filters for the first match
--     test :: (BackendFilter -> a -> Bool) -> Maybe a -> Maybe BackendFilter
--     test f ma = ma >>= \a -> listToMaybe $ filter (`f` a) filters
backendFilter _ _ _ _ b = return [b]

-- | Alter the 'BuildOpts' using the document's 'Meta' and the code
--   block's 'Attr'. Current supported adjustments are:
--
--   * Change size with @width=@ and @size=@ keys or @.absolute@ class:
--
--     @@
--     ``` {.diagram width=300 height=200}
--     -- Or
--     ``` {.diagram .absolute}
--     example = pentagon 100 # fc orange
--     ```
--     @@
--
--   * Don't post-process the diagram with @.no-post-process@ class.
--
--   * Change the expression with @diagram-expression@ 'Meta' value or
--     @'diagram-expression=@ key.
--
--   * Include extra modules with @extra-diagrams-modules@ or
--     @extra-modules@ types in the document 'Meta'. For example, in a
--     markdown header:
--
--     @@
--     ---
--     title: Pretty diagrams
--     extra-diagrams-modules:
--       - Diagrams.TwoD.Sunburst
--       - Diagrams.TwoD.Factorization
--     ...
--
--     Rest of markdown document.
--     @@
--
-- keysOptsAlter :: Meta -> Attr -> DiaBuildOpts -> DiaBuildOpts
-- keysOptsAlter (Meta m) (_ident, classes, keys) b =
--   b & case (lookupRead "width" keys, lookupRead "height" keys) of
--         (Just w, Just h)  -> buildSize .~ dims2D w h
--         (Just w, Nothing) -> buildSize .~ mkWidth w
--         (Nothing, Just h) -> buildSize .~ mkHeight h
--         _                 -> id
--     & whenever ("absolute" `elem` classes) (buildSize .~ absolute)
--     & whenever ("no-post-process" `elem` classes) (postProcess .~ id)
--     & maybe id (set diaExpr) expr
--     & imports <>~ extraMods
--   where
--     extraMods = m ^.. (ix "extra-diagrams-modules" <> ix "extra-modules") . template

--     expr = lookup "diagram-expression" keys
--        <|> m ^? ix "diagram-expression" . template

------------------------------------------------------------------------
-- Pandoc building
------------------------------------------------------------------------

image_ :: String -> String -> FilePath -> Block
image_ inline title path = Para [Image nullAttr [Str inline] (path, title)]

latexInput_ :: String -> Block
latexInput_ path = RawBlock "latex" ("\\input{" ++ path ++ "}")

contextInput_ :: String -> Block
contextInput_ path = RawBlock "context" ("\\input{" ++ path ++ "}")

code_ :: String -> Block
code_ = CodeBlock nullAttr

handleError :: Either String FilePath -> (FilePath -> Block) -> IO Block
handleError d b = case d of
  Left err   -> hPutStrLn stderr "An error occurred! See output for detail."
             >> return (code_ $ "Error!\n" ++ err)
  Right file -> return (b file)

------------------------------------------------------------------------
-- PGF
------------------------------------------------------------------------

-- pgfFilter :: BackendFilter
-- pgfFilter = BackendFilter
--   { nameMatch   = (`elem` ["pgf", "portable-graphics-format"])
--   , formatMatch = (`elem` ["latex", "context", "pdf"])
--   , defaultOpts = with & outputSize .~ defaultSize
--   , filterBuild = addDiagramPgf
--   }

-- pgfBuildOpts :: BuildOpts PGF V2 Double
-- pgfBuildOpts = mkBuildOpts PGF zero with
--                  & diaExpr   .~ "example"
--                  & hashCache ?~ "diagrams"
--                  & buildSize .~ defaultSize
--                  & imports   .~ [ "Diagrams.Backend.PGF"
--                                 , "Diagrams.Prelude"
--                                 ]

-- addDiagramPgf :: Maybe Format -> BuildOpts PGF V2 Double -> Attr -> String -> IO Block
-- addDiagramPgf mf opts_ ats code =
--   case mf of
--     Just (Format "latex") -> do
--       d <- compileDiagram opts code "tex"
--       handleError d latexInput_

--     Just (Format "context") -> do
--       d <- compileDiagram (opts & backendOpts . surface .~ contextSurface) code "tex"
--       handleError d latexInput_

--     _ -> do
--       d <- compileDiagram opts code "pdf"
--       handleError d $ image_ (ats^._1) "diagram"
--   where
--     opts = opts_ & imports <>~ ["Diagrams.Backend.PGF"]

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

-- svgFilter :: BackendFilter
-- svgFilter = BackendFilter
--   { nameMatch   = (`elem` ["svg"])
--   , formatMatch = (`elem` ["html", "md", "markdown"])
--   , defaultOpts = SVGOptions defaultSize []
--   , filterBuild = addDiagramSVG
--   }

-- svgBuildOpts :: BuildOpts SVG V2 Double
-- svgBuildOpts = mkBuildOpts SVG zero (SVGOptions defaultSize [])
--                  & diaExpr   .~ "example"
--                  & hashCache ?~ "diagrams"
--                  & imports   .~ [ "Diagrams.Backend.SVG"
--                                 , "Diagrams.Prelude"
--                                 ]

-- addDiagramSVG :: Maybe Format -> BuildOpts SVG V2 Double -> Attr -> String -> IO Block
-- addDiagramSVG _ opts_ ats code = do
--   d <- compileDiagram opts code "svg"
--   handleError d $ image_ (ats^._1) "diagram"
--   where
--     opts = opts_ & imports <>~ ["Diagrams.Backend.SVG"]

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------

-- type Raster = Rasterific

-- rasterificFilter :: BackendFilter
-- rasterificFilter = BackendFilter
--   { nameMatch   = (`elem` ["rasterific", "raster"])
--   , formatMatch = const False -- Rasterific is the fallback (so first in list)
--   , defaultOpts = RasterificOptions defaultSize
--   , filterBuild = addDiagramRasterific
--   }

-- rasterificBuildOpts :: DiaBuildOpts
-- rasterificBuildOpts = mkBuildOpts Rasterific zero (RasterificOptions defaultSize)
--                  & diaExpr   .~ "example"
--                  & hashCache ?~ "diagrams"
--                  & imports   .~ [ "Diagrams.Backend.Rasterific"
--                                 , "Diagrams.Prelude"
--                                 ]

-- addDiagramRasterific :: Maybe Format -> BuildOpts Rasterific V2 Float -> Attr -> String -> IO Block
-- addDiagramRasterific _ opts_ ats code = do
--   d <- compileDiagram opts code "png"
--   handleError d $ image_ (ats^._1) "diagram"
--   where
--     opts = opts_ & imports <>~ ["Diagrams.Backend.Rasterific"]

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- | @compileDiagram opts src ext@ compiles the literate source code of
--   a diagram to a file with a file name given by a hash of the source
--   code contents. Returns the path to the result or an interpretor /
--   compiler error.
compileDiagram :: DiaBuildOpts -> String -> String -> IO (Either String FilePath)
compileDiagram opts src ext = do
  r <- buildDiaToHash (opts & snippets %~ (++ [src])) ext
  let mkPath h = (opts ^. hashCache . _Just) </> showHash h <.> ext
  return $ case r of
    OK h ()       -> Right $ mkPath h
    Skipped h     -> Right $ mkPath h
    InterpError e -> Left $ ppInterpError e
    ParseError e  -> Left e

lookupRead :: (Eq a, Read b) => a -> [(a, String)] -> Maybe b
lookupRead a = lookup a >=> readMaybe

whenever :: Bool -> (a -> a) -> a -> a
whenever b f = if b then f else id

-- buildSize :: Lens' DiaBuildOpts (SizeSpec V2 Int)
-- buildSize = backendOpts . diaOutputSize
