import           System.FilePath                    ((<.>), (</>))
import           System.IO
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Control.Monad                      (when)
import qualified Diagrams.Builder                   as DB
import           System.Directory                   (createDirectory,
                                                     doesDirectoryExist)
import           Data.VectorSpace                   (zeroV)
import Text.Pandoc.JSON
import           Diagrams.TwoD.Size                 (SizeSpec2D (Dims))
import           Diagrams.Prelude                   (centerXY, pad, (&), (.~))
import Control.Applicative
import Data.List (delete)

-- TODO choose output format based on pandoc target
backendExt :: String
backendExt = "png"

main :: IO ()
main = toJSONFilter $ insertDiagrams "images"

insertDiagrams :: FilePath -> Block -> IO [Block]
insertDiagrams outDir (CodeBlock (ident, classes, attrs) code)
    | "diagram-haskell" `elem` classes = (++ [bl']) <$> img
    | "diagram" `elem` classes = img
  where
    img = do
        d <- compileDiagram outDir code
        return $ case d of
            Left _err     -> []
            Right imgName -> [Plain [Image [] (imgName,"")]] -- no alt text, no title
    bl' = CodeBlock (ident, "haskell":delete "diagram-haskell" classes, attrs) code
insertDiagrams _ block = return [block]

-- Copied from https://github.com/diagrams/diagrams-doc/blob/master/doc/Xml2Html.hs
-- With the CPP removed, thereby requiring Cairo
-- TODO clean this up, move it into -builder somehow
-- | Compile the literate source code of a diagram to a .png file with
--   a file name given by a hash of the source code contents
compileDiagram :: FilePath -> String -> IO (Either String String)
compileDiagram outDir src = do
  ensureDir outDir

  let bopts = DB.mkBuildOpts

                Cairo

                zeroV

                (CairoOptions "default.png" (Dims 500 200) PNG False)

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
                & DB.diaExpr .~ "example"
                & DB.postProcess .~ (pad 1.1 . centerXY)
                & DB.decideRegen .~
                  (DB.hashedRegenerate
                    (\hash opts -> opts { _cairoFileName = mkFile hash })
                    outDir
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
  mkFile base = outDir </> base <.> backendExt
  ensureDir dir = do
    b <- doesDirectoryExist dir
    when (not b) $ createDirectory dir
