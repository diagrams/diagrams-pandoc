{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Diagrams.Pandoc where

import           Control.Lens                hiding ((<.>))
import           Diagrams.Backend.Build
import           Diagrams.Builder            as DB
import           Diagrams.Builder.Opts       as DB
import           Diagrams.Prelude            hiding (block)
import           System.FilePath
import           System.IO
import           Text.Pandoc.JSON

import           Diagrams.Backend.PGF
import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.SVG

defaultBackend :: Maybe Format -> Attr -> String -> IO Block
defaultBackend mf =
  case mf of
    Just (Format "latex") -> addDiagramPGF pgfBuildOpts mf
    Just (Format "html")  -> addDiagramSVG svgBuildOpts mf
    _                     -> addDiagramRasterific rasterificBuildOpts mf

addDiagrams :: Maybe Format -> Block -> IO [Block]
addDiagrams mf (CodeBlock attrs@(_ident, classes, _ats) code)
  | "diagram" `elem` classes = fmap pure dia
  | "raster" `elem` classes  = fmap pure rasterDia
  -- TODO: cases for including code
  where
    dia = defaultBackend mf attrs code
    rasterDia = addDiagramRasterific rasterificBuildOpts mf attrs code

addDiagrams _ block = pure [block]

-- | Alter build options from args including:
--
--   * change size spec via \"width=10\", \"height=20\" or
--     \".absolute\"
--
--   * no post-processing with \".nopostprocess\"
--
--   * no hashing with ".nohash"
--
-- alterOptions :: BackendBuild b v n => BuildOpts b v n -> Attr -> BuildOpts b v n
-- alterOptions b (_ident, classes, _attrs) =

------------------------------------------------------------------------
-- PGF
------------------------------------------------------------------------

pgfBuildOpts :: BuildOpts PGF V2 Double
pgfBuildOpts = mkBuildOpts PGF zero with
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & backendOpts . outputSize .~ dims2D 180 120
                 & imports .~ [ "Diagrams.Backend.PGF"
                              , "Diagrams.Prelude"
                              ]

addDiagramPGF :: BuildOpts PGF V2 Double -> Maybe Format -> Attr -> String -> IO Block
addDiagramPGF opts mf _ats code = do
  d <- compileDiagram opts code "tex"
  case d of
    Left err -> hPutStrLn stderr "An error occured! See output for detail."
             >> return (CodeBlock nullAttr ("Error!\n" ++ err))
    Right file -> case mf of
      Just (Format "latex")
        -> return $ RawBlock "latex" ("\\input{" ++ file ++ "}")
      -- Just (Format "html")
      --   -> do Image [] ""
      _ -> return $ CodeBlock nullAttr ("Error!\n" ++ show mf)

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

svgBuildOpts :: BuildOpts SVG V2 Double
svgBuildOpts = mkBuildOpts SVG zero (SVGOptions (dims2D 180 120) Nothing)
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & backendOpts . outputSize .~ dims2D 220 180
                 & imports .~ [ "Diagrams.Backend.SVG"
                              , "Diagrams.Prelude"
                              ]

addDiagramSVG :: BuildOpts SVG V2 Double -> Maybe Format -> Attr -> String -> IO Block
addDiagramSVG opts _mf ats code = do
  d <- compileDiagram opts code "svg"
  case d of
    Left err -> hPutStrLn stderr "An error occured! See output for detail."
             >> return (CodeBlock nullAttr ("Error!\n" ++ err))
    Right file -> return $ Para [Image [Str $ ats^._1] (file,"diagram")]

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------
type Raster = Rasterific

rasterificBuildOpts :: BuildOpts Raster V2 Float
rasterificBuildOpts = mkBuildOpts Rasterific zero (RasterificOptions (dims2D 180 120))
                 & diaExpr   .~ "example"
                 & hashCache ?~ "diagrams"
                 & backendOpts . outputSize .~ dims2D 180 120
                 & imports .~ [ "Diagrams.Backend.Rasterific"
                              , "Diagrams.Prelude"
                              ]

addDiagramRasterific :: BuildOpts Rasterific V2 Float -> Maybe Format -> Attr -> String -> IO Block
addDiagramRasterific opts _mf ats code = do
  d <- compileDiagram opts code "png"
  case d of
    Left err -> hPutStrLn stderr "An error occured! See output for detail."
             >> return (CodeBlock nullAttr ("Error!\n" ++ err))
    Right file -> return $ Para [Image [Str $ ats^._1] (file,"diagram")]

-- | Compile the literate source code of a diagram to a .png file with
--   a file name given by a hash of the source code contents
compileDiagram :: BuildBackend b v n => BuildOpts b v n -> String -> String -> IO (Either String FilePath)
compileDiagram opts src ext = do
  r <- buildToHash (opts & snippets %~ (++ [src])) ext
  let mkPath h = (opts ^. hashCache . _Just) </> showHash h <.> ext
  return $ case r of
    OK h ()       -> Right $ mkPath h
    Skipped h     -> Right $ mkPath h
    InterpError e -> Left $ ppInterpError e
    ParseError e  -> Left e

