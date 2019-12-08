module Day08
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

parse = head . map (P.run parser)
  where
    parser = do
        xs <- many (satisfy isDigit)
        eof
        return (map (read . (: [])) xs)

chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

solve1 :: [Int] -> String
solve1 xs = show (nInLayer 1 minLayer * nInLayer 2 minLayer)
  where
    layers   = chunksOf (25 * 6) xs
    minLayer = minimumBy (comparing (nInLayer 0)) layers
    nInLayer n = length . filter (== n)

--solve2 :: [String] -> String
solve2 xs = showImg $ foldr appendLayer (repeat 0) layers
  where
    appendLayer = zipWith appendPixel
    appendPixel 2    under = under
    appendPixel next _     = next
    layers = chunksOf (25 * 6) xs
    showImg =
        ("\n" ++) . unlines . map (intersperse ' ') . chunksOf 25 . map pColor
    pColor 0 = ' '
    pColor 1 = '@'

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
