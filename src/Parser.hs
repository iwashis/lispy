module Parser (doubleParser, keywordsParser, booleanParser, literalParser, exprParser, programParser) where

import Control.Applicative (asum)
import Data.Char (isAlpha, isAlphaNum)
import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)

maybeSpaces :: Parser ()
maybeSpaces = do
    _ <- many $ oneOf " \t\n\r" -- Matches spaces, tabs, and newlines
    return ()

lexeme :: Parser a -> Parser a
lexeme p = p <* maybeSpaces

-- Parser for a double value
-- -- Parser for a double value
doubleParser :: Parser Double
doubleParser = do
    -- Parse an optional minus sign, integer part, and optional fractional part
    numberString <- (++) <$> option "" (string "-")
                         <*> ((++) <$> many1 digit
                                   <*> option "" (char '.' >> many1 digit))
    -- Convert the parsed string to Double
    return (read numberString)

keywordsParser :: [String] -> Parser String
keywordsParser symbols = asum $ map tryKeyword symbols
  where
    tryKeyword keyword = try (string keyword <* notFollowedBy validRestChar)
    validRestChar = satisfy isAlphaNum -- Ensures no alphanumeric character follows the keyword

-- Boolean parser using symbolParser
booleanParser :: Parser Bool
booleanParser = do
    result <- keywordsParser ["#t", "#f"]
    case result of
        "#t" -> return True
        "#f" -> return False
        _ -> unexpected "Invalid boolean value" -- Parsec functionality for handling unexpected input

-- Parser for strings not starting with a special character
nameParser :: Parser String
nameParser = do
    first <- validFirstChar
    rest <- many validRestChar -- Parse the rest of the string
    return $ first : rest
  where
    validFirstChar = satisfy (\c -> isAlpha c || c == '_')
    validRestChar = satisfy (\c -> isAlphaNum c || c == '_') -- Allow letters, digits, and underscores

funcNameParser :: Parser String
funcNameParser = lexeme $ nameParser <|> keywordsParser primitiveFunctionSymbols

literalParser :: Parser Literal
literalParser =
    (Number <$> doubleParser)
        <|> (Boolean <$> booleanParser)
        <|> ( StringLiteral <$> do
                _ <- char '"'
                literal <- nameParser
                _ <- char '"'
                pure literal
            )

inBrackets :: Parser a -> Parser a
inBrackets p = do
    _ <- lexeme $ char '('
    value <- p
    _ <- lexeme $ char ')'
    return value

exprParser :: Parser Expr
exprParser =
    (Literal <$> lexeme literalParser)
        <|> inBrackets
            ( ( do
                    _ <- lexeme $ keywordsParser ["lambda"]
                    params <- inBrackets $ many1 $ lexeme nameParser
                    Lambda params <$> exprParser
              )
                <|> ( do
                        _ <- lexeme $ keywordsParser ["def"]
                        varName <- lexeme nameParser
                        Define varName <$> exprParser
                    )
                <|> ( do
                        _ <- lexeme $ keywordsParser ["defun"]
                        funcName <- lexeme nameParser
                        params <- inBrackets $ many1 $ lexeme nameParser
                        DefineProc funcName params <$> exprParser
                    )
                <|> ( do
                        _ <- lexeme $ keywordsParser ["if"]
                        predicate <- exprParser
                        thenExpr <- exprParser
                        elseExpr <- optionMaybe exprParser
                        return $ If predicate thenExpr elseExpr
                    )
                <|> ( do
                        funcName <- lexeme funcNameParser
                        args <- many1 $ lexeme exprParser
                        return $ ProcedureCall funcName args
                    )
            )
        <|> (Var <$> lexeme nameParser)

programParser :: Parser Program
programParser = Program <$> sepEndBy exprParser (lexeme $ char ';')
