{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Diagrams.Pandoc where

import           Control.Lens                hiding ((<.>))
import           Control.Monad
import           Data.List                   (delete)
import           Diagrams.Backend.Build
import           Diagrams.Builder            as DB
import           Diagrams.Prelude            hiding (block)
import           System.FilePath
import           System.IO
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON
import           Text.Read

import           Diagrams.Backend.PGF
import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.SVG

-- | Default size to use when nothing else is given.
defaultSize :: Num n => SizeSpec V2 n
defaultSize = dims2D 260 120

-- | Default filter to use for turning diagrams code blocks in to
--   diagrams.
diaPandocFilter :: Maybe Format -> Pandoc -> IO Pandoc
diaPandocFilter mf (Pandoc m bs) = Pandoc m <$> walk' (addDiagrams mf) bs
  where
    -- backend      = lookupMeta "backend" m
    -- extraImports = lookupMeta "extra-imports" m
    -- expression   = lookupMeta "diagram-expression" m
    walk' f = bottomUpM (liftM concat . mapM f)

defaultBackend :: Maybe Format -> Attr -> String -> IO Block
defaultBackend mf ats =
  case mf of
    Just (Format "latex")   -> addDiagramPGF (alter pgfBuildOpts) mf ats
    Just (Format "context") -> addDiagramPGF (alter pgfBuildOpts) mf ats
    Just (Format "html")    -> addDiagramSVG (alter svgBuildOpts) mf ats
    _                       -> addDiagramRasterific (alter rasterificBuildOpts) mf ats
  where alter = alterOptions ats

addDiagrams :: Maybe Format -> Block -> IO [Block]
addDiagrams mf (CodeBlock attrs@(_ident, classes, _ats) code)
  | "diagram" `elem` classes = fmap pure dia
  | "raster" `elem` classes  = fmap pure diaRaster
  | "diagram-code" `elem` classes = do
       d <- dia
       pure [d, CodeBlock (attrs & _2 %~ delete "diagram-code") code]
  where
    dia       = defaultBackend mf attrs code
    diaRaster = addDiagramRasterific rasterificBuildOpts mf attrs code

addDiagrams _ block = pure [block]

-- | Alter build options from args including:
--
--   * change size spec via \"width=10\", \"height=20\" or
--     \".absolute\"
--
--   * no post-processing with \".nopostprocess\"
--
alterOptions :: (Read n, Num n) => BackendBuild b v n => Attr -> BuildOpts b v n -> BuildOpts b v n
alterOptions (_ident, classes, attrs) b =
  b & case (lookupRead "width" attrs, lookupRead "height" attrs) of
        (Just w, Just h)  -> buildSize .~ dims2D w h
        (Just w, Nothing) -> buildSize .~ mkWidth w
        (Nothing, Just h) -> buildSize .~ mkHeight h
        _                 -> id
    & whenever ("absolute" `elem` classes) (buildSize .~ absolute)
    & whenever ("nopp" `elem` classes) (postProcess .~ id)
    & maybe id (set diaExpr) (lookup "expr" attrs)

lookupRead :: (Eq a, Read b) => a -> [(a, String)] -> Maybe b
lookupRead a = lookup a >=> readMaybe

whenever :: Bool -> (a -> a) -> a -> a
whenever b f = if b then f else id

buildSize :: BackendBuild b v n => Lens' (BuildOpts b v n) (SizeSpec V2 n)
buildSize = backendOpts . outputSize

------------------------------------------------------------------------
-- Pandoc building
------------------------------------------------------------------------

image_ :: String -> String -> FilePath -> Block
image_ inline title path = Para [Image [Str inline] (path, title)]

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

pgfBuildOpts :: BuildOpts PGF V2 Double
pgfBuildOpts = mkBuildOpts PGF zero with
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & buildSize .~ defaultSize
                 & imports   .~ [ "Diagrams.Backend.PGF"
                                , "Diagrams.Prelude"
                                ]

addDiagramPGF :: BuildOpts PGF V2 Double -> Maybe Format -> Attr -> String -> IO Block
addDiagramPGF opts mf ats code =
  case mf of
    Just (Format "latex") -> do
      d <- compileDiagram opts code "tex"
      handleError d latexInput_

    Just (Format "context") -> do
      d <- compileDiagram (opts & backendOpts . surface .~ contextSurface) code "tex"
      handleError d latexInput_

    _ -> do
      d <- compileDiagram opts code "pdf"
      handleError d $ image_ (ats^._1) "diagram"

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

svgBuildOpts :: BuildOpts SVG V2 Double
svgBuildOpts = mkBuildOpts SVG zero (SVGOptions defaultSize Nothing)
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & imports   .~ [ "Diagrams.Backend.SVG"
                                , "Diagrams.Prelude"
                                ]

addDiagramSVG :: BuildOpts SVG V2 Double -> Maybe Format -> Attr -> String -> IO Block
addDiagramSVG opts _mf ats code = do
  d <- compileDiagram opts code "svg"
  handleError d $ image_ (ats^._1) "diagram"

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------

type Raster = Rasterific

rasterificBuildOpts :: BuildOpts Raster V2 Float
rasterificBuildOpts = mkBuildOpts Rasterific zero (RasterificOptions defaultSize)
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & imports   .~ [ "Diagrams.Backend.Rasterific"
                                , "Diagrams.Prelude"
                                ]

addDiagramRasterific :: BuildOpts Rasterific V2 Float -> Maybe Format -> Attr -> String -> IO Block
addDiagramRasterific opts _mf ats code = do
  d <- compileDiagram opts code "png"
  handleError d $ image_ (ats^._1) "diagram"

------------------------------------------------------------------------
-- Compiling
------------------------------------------------------------------------

-- | @compileDiagram opts src ext@ compiles the literate source code of
--   a diagram to a file with a file name given by a hash of the source
--   code contents. Returns the path to the result or an interpretor /
--   compiler error.
compileDiagram :: BackendBuild' b v n => BuildOpts b v n -> String -> String -> IO (Either String FilePath)
compileDiagram opts src ext = do
  r <- buildDiaToHash (opts & snippets %~ (++ [src])) ext
  let mkPath h = (opts ^. hashCache . _Just) </> showHash h <.> ext
  return $ case r of
    OK h ()       -> Right $ mkPath h
    Skipped h     -> Right $ mkPath h
    InterpError e -> Left $ ppInterpError e
    ParseError e  -> Left e

