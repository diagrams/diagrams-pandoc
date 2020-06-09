{-# LANGUAGE CPP #-}

module Main where

import           Text.Pandoc.Diagrams

#if !MIN_VERSION_BASE(4,11,0)
import           Data.Monoid
#endif

import           Options.Applicative
import           Text.Pandoc.JSON

main :: IO ()
main = do
    opts <- execParser withHelp
    toJSONFilter $ insertDiagrams opts

optsParser :: Parser Opts
optsParser = Opts
             <$> strArgument (help "target output format from pandoc" <> value "html")
             <*> strOption (long "out" <> short 'o' <> metavar "DIR"
                            <> help "Directory for image files" <> value "images")
             <*> strOption (long "expression" <> long "expr" <> short 'e' <>
                            metavar "NAME" <>
                            help "name of Diagram value in Haskell snippet" <>
                            value "example")
             <*> switch    (long "absolute" <> short 'a' <>
                            help "output the name of Diagram in Haskell snippet as absolute path")
             <*> option auto (long "backend" <> short 'b' <> metavar "BACKEND" <> value Cairo)

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")
