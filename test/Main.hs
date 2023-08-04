{-# LANGUAGE OverloadedStrings #-}

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

-- We are renaming the image files so that the file names would be deterministic
-- across environments, GHC versions, etc. We are interested in the actual
-- generated image anyway.
renameImageHash ::  Pandoc.Inline -> Pandoc.Inline
renameImageHash (Pandoc.Image attr alt (_, title)) = Pandoc.Image attr alt ("image.png", title)
renameImageHash x = x

mdToHtml :: String -> IO Text
mdToHtml f = do
  mdContents <- T.readFile f
  Pandoc.runIOorExplode $ do
    -- Pandoc extensions that are enabled by default in Pandoc CLI are also
    -- enabled here, because the input files make use of some fancy syntax
    -- (backtick_code_blocks, fenced_code_attributes, etc.)
    ast <- Pandoc.readMarkdown (def { readerExtensions = pandocExtensions }) mdContents
    filteredAst <- liftIO $ diagramsFilter ast
    let renamedAst = Pandoc.walk renameImageHash filteredAst
    Pandoc.writeHtml5String (def { writerExtensions = pandocExtensions }) renamedAst

goldenTests :: IO TestTree
goldenTests = do
  mdFiles <- Tasty.findByExtension [".md"] "test"
  pure $ Tasty.testGroup "golden tests"
    [ Tasty.goldenVsString
        (takeBaseName mdFile)
        htmlFile
        (LBS.fromStrict . T.encodeUtf8 <$> mdToHtml mdFile)
    | mdFile <- mdFiles
    , let htmlFile = replaceExtension mdFile ".html"
    ]
