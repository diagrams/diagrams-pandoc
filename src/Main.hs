module Main where

import           Text.Pandoc.Diagrams

import           Options.Applicative
import           Text.Pandoc.JSON

main :: IO ()
main = do
    opts <- execParser withHelp
    toJSONFilter $ insertDiagrams opts

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (long "out" <> short 'o' <> metavar "DIR"
                            <> help "Directory for image files" <> value "images")
             <*> strOption (long "expression" <> long "expr" <> short 'e' <>
                            metavar "NAME" <>
                            help "name of Diagram value in Haskell snippet" <>
                            value "example")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")
