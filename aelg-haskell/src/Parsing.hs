module Parsing
    ( integer
    , integerAnd
    , integer'
    , integerAnd'
    , run
    , listOf
    )
where

import           Data.Char
import           Text.ParserCombinators.ReadP

integer :: ReadP Int
integer =
    skipSpaces
        >>  read
        <$> ((++) <$> option "" (string "-") <*> many1 (satisfy isDigit))

integerAnd :: ReadP a -> ReadP Int
integerAnd r = integer >>= \n -> r >> return n

integer' :: ReadP Integer
integer' =
    skipSpaces
        >>  read
        <$> ((++) <$> option "" (string "-") <*> many1 (satisfy isDigit))

integerAnd' :: ReadP a -> ReadP Integer
integerAnd' r = integer' >>= \n -> r >> return n

listOf :: ReadP a -> ReadP [a]
listOf a = do
    x  <- a
    xs <- many (char ',' >> a)
    return (x : xs)

run :: ReadP a -> String -> a
run parser s = fst . head $ readP_to_S parser s
