import Text.Pandoc.JSON
import Diagrams.Pandoc

main :: IO ()
main = toJSONFilter defFilter

defFilter :: Maybe Format -> Pandoc -> IO Pandoc
defFilter = pandocFilter (backendFilter id defaultFilters)

