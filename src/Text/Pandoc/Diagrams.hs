{-# LANGUAGE CPP #-}

-- | Convert appropriately annotated Code blocks to an image, with or
-- without display of the code.  Interpret the Code blocks as Haskell
-- code using the Diagrams libraries.

module Text.Pandoc.Diagrams where

import           Control.Lens                    (Lens', (&), (.~), (<>~))
import           Data.Char                       (toLower)
import           Data.List                       (delete)
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import qualified Diagrams.Builder                as DB
import           Diagrams.Prelude                (Options, Result, centerXY,
                                                  pad)
import           Diagrams.Size                   (dims)
import           Linear                          (V2 (..), zero)
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath                 ((<.>), (</>))
import           System.IO
import           Text.Pandoc.Definition

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

backendExt :: String -> String
backendExt "beamer" = "pdf"
backendExt "latex" = "pdf"
backendExt _ = "png"

-- Return output type for a string
findOutputType :: String -> OutputType
findOutputType "beamer" = PDF
findOutputType "latex" = PDF
findOutputType _ = PNG

data Opts = Opts {
    _outFormat  :: String,
    _outDir     :: FilePath,
    _expression :: String
    }

data Snippet = Snippet {
  _opts  :: Opts,
  _attrs :: [(String,String)],
  _src   :: String
  }

data Echo = Above | Below

insertDiagrams :: Opts -> Block -> IO [Block]
insertDiagrams opts (CodeBlock (ident, classes, attrs) code)
    | "diagram-haskell" `elem` classes = do
      i <- img
      return $ case echo of
        Above -> [bl', i]
        Below -> [i, bl']
    | "diagram" `elem` classes = (:[]) <$> img
  where
    img = do
        d <- compileDiagram $ Snippet opts attrs code
        return $ case d of
            Nothing     -> Null  -- already reported error on stderr
            Just imgName -> Plain [Image [] (imgName,"")] -- no alt text, no title
    bl' = CodeBlock (ident, "haskell":delete "diagram-haskell" classes, attrs) code
    echo = readEcho attrs
insertDiagrams _ block = return [block]

-- Copied from https://github.com/diagrams/diagrams-doc/blob/master/doc/Xml2Html.hs
-- With the CPP removed, thereby requiring Cairo
-- TODO clean this up, move it into -builder somehow
-- | Compile the literate source code of a diagram to a .png/.pdf file with
--   a file name given by a hash of the source code contents
compileDiagram :: Snippet -> IO (Maybe String)
compileDiagram snip = do
  createDirectoryIfMissing True . _outDir . _opts $ snip

  let
      bopts :: DB.BuildOpts Cairo V2 Double
      bopts = DB.mkBuildOpts Cairo zero
                ( CairoOptions "default.png"
                  dimensions
                  (findOutputType . _outFormat . _opts $ snip)
                  False
                ) & DB.imports .~
                  [ "Diagrams.Backend.Cairo"
                  , "Diagrams.Backend.Cairo.Internal"
                  ] & snippetBuildOpts snip

  handleResult snip =<< DB.buildDiagram bopts
  where
    dimensions = dims $ V2 (widthAttribute $ _attrs snip) (heightAttribute $ _attrs snip)

snippetBuildOpts :: FileBackend b =>
  Snippet -> DB.BuildOpts b V2 Double -> DB.BuildOpts b V2 Double
snippetBuildOpts snip bopts =
  bopts
  & DB.snippets .~ [_src snip]
  & DB.imports  <>~
  [ -- "Diagrams.TwoD.Types"      -- WHY IS THIS NECESSARY =(
    -- , "Diagrams.Core.Points"
    -- GHC 7.2 bug?  need  V (Point R2) = R2  (see #65)
    "Graphics.SVGFonts"
  , "Data.Typeable"
  ]
  & DB.pragmas .~ ["DeriveDataTypeable"]
  & DB.diaExpr .~ _expression (_opts snip)
  & DB.postProcess .~ (pad 1.1 . centerXY)
  & DB.decideRegen .~ DB.hashedRegenerate
  (\hash -> backendFileName .~ mkFile snip hash )
  (_outDir $ _opts snip)

handleResult :: FileBackend b =>
                Snippet -> DB.BuildResult b V2 Double -> IO (Maybe String)
handleResult snip res = case res of
    DB.ParseErr err    -> do
      hPutStrLn stderr ("\nError while parsing\n" ++ _src snip)
      hPutStrLn stderr err
      return Nothing

    DB.InterpErr ierr  -> do
      hPutStrLn stderr ("\nError while interpreting\n" ++ _src snip)
      hPutStrLn stderr (DB.ppInterpError ierr)
      return Nothing

    DB.Skipped hash    -> do
      hPutStr stderr "."
      hFlush stderr
      return $ Just (mkFile snip (DB.hashToHexStr hash))

    DB.OK hash out -> do
      hPutStr stderr "O"
      hFlush stderr
      renderToFile res out
      return $ Just (mkFile snip (DB.hashToHexStr hash))

mkFile :: Snippet -> FilePath -> FilePath
mkFile snip base =
  _outDir (_opts snip) </> base <.> (backendExt . _outFormat . _opts $ snip)

widthAttribute :: [(String,String)] -> Double
widthAttribute attrs =
    case lookup "width" attrs of
        Nothing -> 500
        Just v  -> read v :: Double

heightAttribute :: [(String,String)] -> Double
heightAttribute attrs =
    case lookup "height" attrs of
        Nothing -> 200
        Just v  -> read v :: Double

readEcho :: [(String, String)] -> Echo
readEcho attrs = case lookup "echo" attrs of
  Nothing -> Below
  Just v -> case map toLower v of
    "above" -> Above
    _ -> Below

-- | Backends for which diagrams-pandoc can render a Diagram to disk
-- with a specified filename.
class FileBackend b where
  -- The first argument of each is a proxy to fix the type b
  -- Note that Result is a type alias, so doesn't fix b
  renderToFile :: DB.BuildResult b V2 Double -> Result b V2 Double -> IO ()
  backendFileName :: Lens' (Options b V2 Double) String

instance FileBackend Cairo where
  renderToFile = const fst
  backendFileName = cairoFileName
