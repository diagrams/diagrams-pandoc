module Main where

import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Tasty

import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Options (def, readerExtensions, pandocExtensions, writerExtensions)
import qualified Text.Pandoc.Walk as Pandoc

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Directory (removeDirectoryRecursive)
import System.FilePath (replaceExtension, takeBaseName)

import Text.Pandoc.Diagrams (Opts (..), Backend (..), insertDiagrams)


main :: IO ()
main = Tasty.defaultMain . withCleanup =<< goldenTests

-- Cleanup the generated "images" directory after the test runs complete
withCleanup :: TestTree -> TestTree
withCleanup tt = Tasty.withResource (pure ()) (const cleanup) (const tt)
  where
    cleanup :: IO ()
    cleanup = removeDirectoryRecursive (_outDir defaultOpts)

-- Extracted from the CLI parser
defaultOpts :: Opts
defaultOpts = Opts
  { _outFormat = "html"
  , _outDir = "images"
  , _expression = "example"
  , _absolutePath = False
  , _backend = Cairo
  }

diagramsFilter :: Pandoc.Pandoc -> IO Pandoc.Pandoc
diagramsFilter = Pandoc.walkM (fmap concat . mapM (insertDiagrams defaultOpts))

mdToNative :: String -> IO Text
mdToNative f = do
  mdContents <- T.readFile f
  Pandoc.runIOorExplode $ do
    -- Pandoc extensions that are enabled by default in Pandoc CLI are also
    -- enabled here, because the input files make use of some fancy syntax
    -- (backtick_code_blocks, fenced_code_attributes, etc.)
    ast <- Pandoc.readMarkdown (def { readerExtensions = pandocExtensions }) mdContents
    filteredAst <- liftIO $ diagramsFilter ast
    Pandoc.writeNative (def { writerExtensions = pandocExtensions }) filteredAst

goldenTests :: IO TestTree
goldenTests = do
  mdFiles <- Tasty.findByExtension [".md"] "test"
  pure $ Tasty.testGroup "golden tests"
    [ Tasty.goldenVsString
        (takeBaseName mdFile)
        nativeFile
        (LBS.fromStrict . T.encodeUtf8 <$> mdToNative mdFile)
    | mdFile <- mdFiles
    , let nativeFile = replaceExtension mdFile ".out"
    ]
