import           Control.Applicative
import           Control.Monad                   (when)
import           Data.List                       (delete)
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
import           Text.Pandoc.Diagrams
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
