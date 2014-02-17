import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A
import Debug.Trace
import Data.Maybe
import qualified Control.Monad as M
import Text.Regex.Posix

main = do
  c <- getContents
  case parse cFile "" c of
    Right sList -> (mapM_ putStr $ filter (\s -> not $ s =~ ".*break.*") sList) >> putStrLn ""
    Left err -> print err

cFile :: GenParser Char st [String]
cFile =
    A.liftA concat (many switchOr)

switchOr = (try switch) <|> (anyChar >> return [])

switch = (string "switch") >> spaces >> switchCond >> spaces >> switchBlock

switchCond = parenthesizedExpr
parenthesizedExpr = string "(" >> (many (try parenthesizedOr) >> return []) >> string ")"
parenthesizedOr = (try (parenthesizedExpr >> return "")) <|> many1 (noneOf "()")

curlyParenthesizedExpr = string "{" >> (many (try curlyParenthesizedOr) >> return []) >> string "}"
curlyParenthesizedOr = (try (curlyParenthesizedExpr >> return "")) <|> many1 (noneOf "{}")

xCurlyParenthesizedExpr = string "{" >> A.liftA concat (many (try xCurlyParenthesizedOr)) A.<* string "}"
xCurlyParenthesizedOr = (try xCurlyParenthesizedExpr) <|> many1 (noneOf "{}")
                       
switchBlock= string "{" >> spaces >> sCaseOrDefault >> manyTill inSwitchBlock (try (string "}"))

inSwitchBlock = A.liftA concat (manyTill inExpr (try sCaseOrDefault))
sCaseOrDefault = (try (string "case" >> many (noneOf ":") >> string ":")) <|>
                 (try (string "default:")) <|>
                 ((try . lookAhead) (string "}"))

inExpr = (try whileExpr) <|> (try forExpr) <|> (try xCurlyParenthesizedExpr) <|> A.liftA M.return anyChar
-- ignore while and for expressions (they may be eating up the break;
whileExpr = (string "while") >> spaces >> parenthesizedExpr >> spaces >> curlyParenthesizedExpr >> return ""
forExpr = (string "for") >> spaces >> parenthesizedExpr >> spaces >> curlyParenthesizedExpr >> return ""
