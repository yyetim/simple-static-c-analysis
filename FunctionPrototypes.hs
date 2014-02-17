import Text.ParserCombinators.Parsec
import qualified Control.Applicative as A
import Debug.Trace
import Data.Maybe
import qualified Control.Monad as M
import Text.Regex.Posix

main = do
  c <- getContents
  case parse cFile "" c of
    Right sList -> mapM_ (\f -> if f /= "" then putStrLn (clearupName f ++ ";") else return ()) sList
    Left err -> print err

clearupName f = 
  let replaceNewlines = map (\c -> if c == '\n' || c == '\t' then ' ' else c) f
      replaceMultipleSpaces [] last = []
      replaceMultipleSpaces x ' ' = if (head x) == ' ' then replaceMultipleSpaces (tail x) (head x) 
                                    else (head x) : replaceMultipleSpaces (tail x) (head x)
      replaceMultipleSpaces x _ = (head x) : replaceMultipleSpaces (tail x) (head x)
  in 
    replaceMultipleSpaces replaceNewlines '.'

cFile :: GenParser Char st [String]
cFile =
    (many (mergedLine A.<* spaces)) A.<* eof

-- some definitions for generic lines
-- for preprocessor
lineNoBackslash = many (noneOf "\n\\")
skipEol = char '\\' >> newline
mergedLineNoBackslash = lineNoBackslash >> many (skipEol >> lineNoBackslash)
-- c comments
cComment1= string "/*" >> manyTill anyChar (try (string "*/")) >> return ""
cComment2 = string "//" >> manyTill anyChar (try (char '\n')) >> return ""
bracketFreeTextWithComments = A.liftA concat $ many1 (try (cComment1 >> return "")
                                    <|> try (cComment2 >> return "")
                                    <|> try (noneOf "{}" >>= (\c -> return [c])))
bracketSemicolonFreeTextWithComments = A.liftA concat $ many1 (try (cComment1 >> return "")
                                    <|> try (cComment2 >> return "")
                                    <|> try (noneOf ";{}" >>= (\c -> return [c])))

-- curly parenthesized expressions
curlyParenthesizedExpr = string "{" >> many (try curlyParenthesizedOr) >> string "}" >> return ""
curlyParenthesizedOr = (try (curlyParenthesizedExpr >> return "")) <|> (bracketFreeTextWithComments >> return "")

-- specific line definitions
preprocessorLine = spaces >> char '#' >> mergedLineNoBackslash >> return ""
statementLine = bracketSemicolonFreeTextWithComments >> char ';' >> return ""
function = functionPrototype A.<* functionBody
functionPrototype = bracketFreeTextWithComments
functionBody = curlyParenthesizedExpr

-- a merged line
mergedLine = try cComment1 <|> try cComment2 <|> try preprocessorLine <|> try statementLine <|> try function
