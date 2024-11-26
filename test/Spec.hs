import ParserTests (prop_parseDoubleMatchesRead, prop_keywordsParserCorrect, prop_validBooleanStrings)
import Test.QuickCheck
import Data.Char (isAlpha)
import Control.Monad (foldM)
-- Main function to run the tests


nonEmptyAlphaString :: Gen String
nonEmptyAlphaString = suchThat arbitrary (\string ->  (not . null) string && ( foldl (&&) True $ map isAlpha string) )

main :: IO ()
main = do
    putStrLn "Running QuickCheck tests..."
    quickCheck prop_parseDoubleMatchesRead
    -- quickCheck $ forAll nonEmptyAlphaString $ \input ->
    --     forAll (listOf nonEmptyAlphaString) $ \keywords ->
    --         prop_keywordsParserCorrect keywords input
    quickCheck prop_validBooleanStrings 
