{-# LANGUAGE ScopedTypeVariables #-}

module ParserTests where

import Text.Parsec
import Parser (doubleParser, keywordsParser, booleanParser) -- Import your parser from src/Parser.hs
import GHC.Base (Double)
import Test.QuickCheck
import Data.Char (isAlphaNum)


-- Reuse parseDouble if it's in Parser.hs, or redefine it here
parseDouble :: String -> Either ParseError Double
parseDouble = parse doubleParser ""

genStandardDouble :: Gen Double
genStandardDouble = suchThat arbitrary isInStandardNotation
  where 
    isInStandardNotation x = 
      let str = show x 
      in not ('e' `elem` str || 'E' `elem` str)

prop_parseDoubleMatchesRead :: Property
prop_parseDoubleMatchesRead = forAll genStandardDouble $ \x ->
    let input = show x
    in case parseDouble input of
        Right parsed -> parsed == x
        Left _       -> False -- Parsing should not fail for valid Double strings


prop_keywordsParserCorrect :: [String] -> String -> Bool
prop_keywordsParserCorrect keywords input =
    case parse (keywordsParser keywords) "" input of
        Right matched -> matched `elem` keywords && isStandalone matched input
        Left _        -> all (\kw -> not (isStandalone kw input)) keywords
  where
    -- Checks if the keyword is a standalone match in the input
    isStandalone kw inp =
        let (before, rest) = splitAt (length kw) inp
         in before == kw && (null rest || not (isValidContinuation (head rest)))

    -- Valid continuation characters for a longer identifier
    isValidContinuation c = isAlphaNum c || c == '_'


-- Property: Valid boolean strings should parse correctly
prop_validBooleanStrings :: Bool
prop_validBooleanStrings =
    all (\(input, expected) -> parse booleanParser "" input == Right expected)
        [("#t", True), ("#f", False)]

