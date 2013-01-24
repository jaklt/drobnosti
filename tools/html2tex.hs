import Text.Parsec
import Text.Parsec.String
import System.Environment
import Data.Char

getTag :: Parser String
getTag = do
    char '<'
    many $ noneOf "<>"
    char '>'
    return ""

parseAnyChar :: Parser String
parseAnyChar = (\c -> [c]) `fmap` anyChar

parseEntity :: Parser String
parseEntity = do
    char '&'
    s <- many $ noneOf "&;"
    char ';'

    return $ case s of
        "nbsp" -> "~"
        "shy"  -> ""
        "amp"  -> "\\&"
        "lt"   -> "<"
        "gt"   -> ">"
        "apos" -> "'"
        "quot" -> "\""

parseDash :: Parser String
parseDash = char '–' >> return "--"

mainParser :: Parser String
mainParser = concat `fmap` many run
    where
        run =  getTag
           <|> parseDash
           <|> parseEntity
           <|> parseAnyChar

trim :: String -> String
trim = reverse . clean . reverse . clean
    where clean = dropWhile isSpace

main :: IO ()
main = do
    (file:_) <- getArgs
    s <- readFile file

    case parse mainParser "" s of
        Left  r -> putStrLn $ show r
        Right r -> writeFile (file ++ ".tex") $ trim  r
