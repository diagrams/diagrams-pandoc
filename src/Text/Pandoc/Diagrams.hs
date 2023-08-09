{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

-- | Convert appropriately annotated Code blocks to an image, with or
-- without display of the code.  Interpret the Code blocks as Haskell
-- code using the Diagrams libraries.

module Text.Pandoc.Diagrams where

import           Data.Hashable                   (Hashable)
import           Data.List                       (delete)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Typeable                   (Typeable)
import qualified Diagrams.Backend.Cairo.Internal as BCairo
import qualified Diagrams.Backend.SVG            as BSvg
import qualified Diagrams.Builder                as DB
import qualified Diagrams.Core                   as DC
import           Diagrams.Prelude                (centerXY, pad, (&), (.~))
import           Diagrams.Size                   (dims)
import qualified Graphics.Svg                    as Svg
import           Linear                          (V2 (..), zero)
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath                 (pathSeparator, (<.>), (</>))
import           System.IO                       (hFlush, hPutStr, hPutStrLn, stderr)
import           Text.Pandoc.Definition          (Block(..), Caption, Attr)
import qualified Text.Pandoc.Builder as PB
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import           Text.Pandoc.Options             (def, readerExtensions, pandocExtensions)
import           Data.Maybe                      (fromMaybe)
import           Data.Foldable                   (fold)

backendExt :: Opts -> String
backendExt Opts {_backend = SVG } = "svg"
backendExt Opts {_backend = Cairo, ..} = case _outFormat of
  "beamer" -> "pdf"
  "latex"  -> "pdf"
  _        -> "png"

-- Return output type for a string
findCairoOutputType :: String -> BCairo.OutputType
findCairoOutputType "beamer" = BCairo.PDF
findCairoOutputType "latex"  = BCairo.PDF
findCairoOutputType _        = BCairo.PNG

data Opts = Opts {
    _outFormat    :: String,
    _outDir       :: FilePath,
    _expression   :: String,
    _absolutePath :: Bool,
    _backend      :: Backend
    }

data Backend = Cairo | SVG deriving (Read)

data Echo = Above | Below

insertDiagrams :: Opts -> Block -> IO [Block]
insertDiagrams opts@Opts{..} (CodeBlock (ident, classes, attrs) code)
    | "diagram-haskell" `elem` classes = do
      i <- PB.toList <$> img
      return $ case echo of
        Above -> bl' : i
        Below -> i <> [bl']
    | "diagram" `elem` classes = PB.toList <$> img
  where
    img = do
      d <- compileDiagram opts attrs code
      case d of
        Left _err     -> pure mempty  -- TODO log an error here
        Right imgName -> case captionRaw of
          Just captionRaw' -> do
            captionBlocks <- parseCaption captionRaw'
            let caption = blocksToCaption captionBlocks
                alt = fromMaybe (blocksToAlt captionBlocks) altAttr
            pure $ figureWithCaption
              -- transfer identifier from code block to figure, so that
              -- it can be referenced by `pandoc-crossref` and the like.
              (ident, [], [])
              caption
              (if _absolutePath then T.cons pathSeparator imgName else imgName)
              ""
              alt
          Nothing ->
            pure $ PB.plain $ PB.imageWith
              (ident, [], [])
              (if _absolutePath then T.cons pathSeparator imgName else imgName)
              ""
              (fold altAttr)
    bl' = CodeBlock (ident, "haskell":delete "diagram-haskell" classes, attrs) code
    echo = readEcho attrs
    captionRaw = lookup "caption" attrs
    altAttr = PB.str <$> lookup "alt" attrs
insertDiagrams _ block = return [block]

figureWithCaption :: Attr -> Caption -> Text -> Text -> PB.Inlines -> PB.Blocks
figureWithCaption attr caption url title alt =
  PB.figure caption . PB.plain $ PB.imageWith attr url title alt

blocksToCaption :: [Block] -> Caption
blocksToCaption = PB.simpleCaption . PB.fromList

blocksToAlt :: [Block] -> PB.Inlines
blocksToAlt = Pandoc.blocksToInlines'

parseCaption :: Text -> IO [Block]
parseCaption s = Pandoc.runIOorExplode $ do
  (Pandoc.Pandoc _ blks) <-
    Pandoc.readMarkdown (def { readerExtensions = pandocExtensions }) s
  pure blks

-- Copied from https://github.com/diagrams/diagrams-doc/blob/master/doc/Xml2Html.hs
-- With the CPP removed, thereby requiring Cairo
-- TODO clean this up, move it into -builder somehow
-- | Compile the literate source code of a diagram to a .png/.pdf file with
--   a file name given by a hash of the source code contents
compileDiagram :: Opts -> [(Text,Text)] -> Text -> IO (Either String Text)
compileDiagram opts attrs src = do
  ensureDir $ _outDir opts
  case mkBuildOpts opts attrs src of
    SomeBuildOpts bo -> do
      res <- DB.buildDiagram bo
      case res of
        DB.ParseErr err    -> do
          hPutStrLn stderr ("\nError while parsing\n" ++ T.unpack src)
          hPutStrLn stderr err
          return $ Left "Error while parsing"

        DB.InterpErr ierr  -> do
          hPutStrLn stderr ("\nError while interpreting\n" ++ T.unpack src)
          hPutStrLn stderr (DB.ppInterpError ierr)
          return $ Left "Error while interpreting"

        DB.Skipped hash    -> do
          hPutStr stderr "."
          hFlush stderr
          return $ Right (T.pack $ mkFile opts (DB.hashToHexStr hash))

        DB.OK hash out -> do
          hPutStr stderr "O"
          hFlush stderr
          let path = mkFile opts (DB.hashToHexStr hash)
          handleResult path $ SomeResult out
          return $ Right (T.pack path)
  where
    ensureDir = createDirectoryIfMissing True
    handleResult path (SomeResult a) = mkImage path a

mkFile :: Opts -> FilePath -> FilePath
mkFile opts base = _outDir opts </> base <.> backendExt opts

data SomeResult = forall r. (MkImage r) => SomeResult r

data SomeBuildOpts v n =
  forall a. (Typeable a, DC.Backend a v n, Hashable (DC.Options a v n), MkImage (DC.Result a v n))
  => SomeBuildOpts (DB.BuildOpts a v n)

class MkImage a where
  mkImage :: FilePath -> a -> IO ()

instance MkImage (IO (), r) where
  mkImage _ = fst

instance MkImage Svg.Element where
  mkImage path e = writeFile path $ show e

mkBuildOpts :: Opts -> [(Text, Text)] -> Text -> SomeBuildOpts V2 Double
mkBuildOpts opts attrs src = case _backend opts of
  Cairo -> SomeBuildOpts $ DB.mkBuildOpts BCairo.Cairo zero
    ( BCairo.CairoOptions "default.png"
      (dims $ V2 (widthAttribute attrs) (heightAttribute attrs))
      (findCairoOutputType $ _outFormat opts)
      False
    )
    & DB.snippets .~ [T.unpack src]
    & DB.imports  .~
      [ "Diagrams.TwoD.Types" -- WHY IS THIS NECESSARY =(
      , "Diagrams.Core.Points" -- GHC 7.2 bug?  need  V (Point R2) = R2  (see #65)
      , "Diagrams.Backend.Cairo"
      , "Diagrams.Backend.Cairo.Internal"
      , "Graphics.SVGFonts"
      , "Data.Typeable"
      ]
    & DB.pragmas .~ ["DeriveDataTypeable"]
    & DB.diaExpr .~ _expression opts
    & DB.postProcess .~ postProcess
    & DB.decideRegen .~
      DB.hashedRegenerate
        (\hash opts' -> opts' { BCairo._cairoFileName = mkFile opts hash })
        (_outDir opts)
  SVG -> SomeBuildOpts $ DB.mkBuildOpts BSvg.SVG zero
    (BSvg.SVGOptions (dims $ V2 (widthAttribute attrs) (heightAttribute attrs)) Nothing "" [] True)
    & DB.snippets .~ [T.unpack src]
    & DB.imports  .~
      [ "Diagrams.TwoD.Types"
      , "Diagrams.Core.Points"
      , "Diagrams.Backend.SVG"
      , "Graphics.SVGFonts"
      , "Data.Typeable"
      ]
    & DB.pragmas .~ ["DeriveDataTypeable"]
    & DB.diaExpr .~ _expression opts
    & DB.postProcess .~ postProcess
  where
    postProcess = pad 1.1 . centerXY

widthAttribute :: [(Text,Text)] -> Double
widthAttribute attrs =
    case lookup "width" attrs of
        Nothing -> 500
        Just v  -> read (T.unpack v) :: Double

heightAttribute :: [(Text,Text)] -> Double
heightAttribute attrs =
    case lookup "height" attrs of
        Nothing -> 200
        Just v  -> read (T.unpack v) :: Double

readEcho :: [(Text, Text)] -> Echo
readEcho attrs = case lookup "echo" attrs of
  Nothing -> Below
  Just v -> case T.toLower v of
    "above" -> Above
    _       -> Below

