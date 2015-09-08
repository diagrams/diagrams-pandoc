
module Text.Pandoc.Diagrams where

import           Control.Applicative
import           Control.Monad                   (when)
import           Data.List                       (delete)
import           Data.Monoid
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import qualified Diagrams.Builder                as DB
import           Diagrams.Prelude                (centerXY, pad, (&), (.~))
import           Diagrams.Size                   (dims)
import           Linear                          (V2 (..), zero)
import           Options.Applicative
import           System.Directory                (createDirectory,
                                                  doesDirectoryExist)
import           System.FilePath                 ((<.>), (</>))
import           System.IO
import           Text.Pandoc.Builder
import           Text.Pandoc.JSON

-- TODO choose output format based on pandoc target
backendExt :: String
backendExt = "png"

data Opts = Opts {
    _outDir     :: FilePath,
    _expression :: String
    }

insertDiagrams :: Opts -> Block -> IO Block
insertDiagrams opts (CodeBlock (ident, classes, attrs) code)
    | "diagram-haskell" `elem` classes = error "not supported"
    | "diagram" `elem` classes = img
  where
    img = do
        d <- compileDiagram opts code
        return $ case d of
            Left _err     -> error "diagram not compiling"
            Right imgName -> Plain [Image [] (imgName,"")] -- no alt text, no title
    bl' = CodeBlock (ident, "haskell":delete "diagram-haskell" classes, attrs) code
insertDiagrams _ block = return block

-- Copied from https://github.com/diagrams/diagrams-doc/blob/master/doc/Xml2Html.hs
-- With the CPP removed, thereby requiring Cairo
-- TODO clean this up, move it into -builder somehow
-- | Compile the literate source code of a diagram to a .png file with
--   a file name given by a hash of the source code contents
compileDiagram :: Opts -> String -> IO (Either String String)
compileDiagram opts src = do
  ensureDir $ _outDir opts

  let
      bopts :: DB.BuildOpts Cairo V2 Double
      bopts = DB.mkBuildOpts

                Cairo

                zero

                (CairoOptions "default.png" (dims $ V2 500 200) PNG False)

                & DB.snippets .~ [src]
                & DB.imports  .~
                  [ "Diagrams.TwoD.Types"      -- WHY IS THIS NECESSARY =(
                  , "Diagrams.Core.Points"
                      -- GHC 7.2 bug?  need  V (Point R2) = R2  (see #65)
                  , "Diagrams.Backend.Cairo"
                  , "Diagrams.Backend.Cairo.Internal"
                  , "Graphics.SVGFonts"
                  , "Data.Typeable"
                  ]
                & DB.pragmas .~ ["DeriveDataTypeable"]
                & DB.diaExpr .~ _expression opts
                & DB.postProcess .~ (pad 1.1 . centerXY)
                & DB.decideRegen .~
                  (DB.hashedRegenerate
                    (\hash opts' -> opts' { _cairoFileName = mkFile hash })
                    (_outDir opts)
                  )

  res <- DB.buildDiagram bopts

  case res of
    DB.ParseErr err    -> do
      hPutStrLn stderr ("\nError while parsing\n" ++ src)
      hPutStrLn stderr err
      return $ Left "Error while parsing"

    DB.InterpErr ierr  -> do
      hPutStrLn stderr ("\nError while interpreting\n" ++ src)
      hPutStrLn stderr (DB.ppInterpError ierr)
      return $ Left "Error while interpreting"

    DB.Skipped hash    -> do
      hPutStr stderr "."
      hFlush stderr
      return $ Right (mkFile (DB.hashToHexStr hash))

    DB.OK hash out -> do
      hPutStr stderr "O"
      hFlush stderr
      fst out
      return $ Right (mkFile (DB.hashToHexStr hash))

 where
  mkFile base = _outDir opts </> base <.> backendExt
  ensureDir dir = do
    b <- doesDirectoryExist dir
    when (not b) $ createDirectory dir
