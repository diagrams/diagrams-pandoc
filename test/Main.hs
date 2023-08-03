{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Test.Tasty as Tasty
import Test.Tasty (TestTree)
import qualified Test.Tasty.Golden as Tasty
import qualified Text.Pandoc as Pandoc
import System.FilePath (replaceExtension, takeBaseName)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Text.Pandoc.Diagrams (Opts(..), Backend (..), insertDiagrams)
import Text.Pandoc.Options (def, readerExtensions, pandocExtensions, writerExtensions)
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Pandoc.Walk as Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (removeDirectoryRecursive)

main :: IO ()
main = do
  tests <- goldenTests
  let tests' = Tasty.withResource (pure ()) (const cleanup) (const tests)
  Tasty.defaultMain tests'
  where
    cleanup :: IO ()
    cleanup = removeDirectoryRecursive (_outDir defaultOpts)

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

mdToNative :: String -> IO T.Text
mdToNative f = do
  mdContents <- T.readFile f
  Pandoc.runIOorExplode $ do
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
        (LBS.fromStrict . encodeUtf8 <$> mdToNative mdFile)
    | mdFile <- mdFiles
    , let nativeFile = replaceExtension mdFile ".out"
    ]
