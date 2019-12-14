module Day14
    ( solve
    )
where

import           Control.Arrow
import           Data.Char
import qualified Data.Map.Strict              as M
import           Data.Maybe
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

chemical = do
    skipSpaces
    n <- P.integer
    string " "
    name <- many1 (satisfy isAlpha)
    return (n, name)

reaction = do
    input <- P.listOf chemical
    string " =>"
    output <- chemical
    eof
    return (input, output)

parse = map $ P.run reaction

buildM = foldl go M.empty
    where go m (input, (n, output)) = M.insert output (input, (n, output)) m

maxOre = 1000000000000

run m (inv, done) | M.size inv == 1 && M.member "ORE" inv = (inv, done)
                  | otherwise = run m (M.foldrWithKey' push (M.empty, done) inv)
  where
    push "ORE" a (b, done1) =
        (M.alter (Just . (+ a) . fromMaybe 0) "ORE" b, done1)
    push k a (b, done1)
        | willBeProduced m k done
        = (M.alter (Just . (+ a) . fromMaybe 0) k b, done1)
        | otherwise
        = (foldr go b ingredients, k : done1)
      where
        (ingredients, (n, _)) = m M.! k
        nRuns = if a `rem` n == 0 then a `div` n else a `div` n + 1
        go (n, name) = M.alter (Just . (+ (nRuns * n)) . fromMaybe 0) name


willBeProduced m name done = M.foldr go False m
  where
    go (ingredients, (_, curName)) b
        | curName `elem` done = b
        | otherwise           = b || (name `elem` map snd ingredients)

solve1 x = show . fromMaybe (-1) . M.lookup "ORE" . fst $ run
    m
    (M.fromList [("FUEL", 1)], [])
    where m = buildM x

possible m fuel = (<= maxOre) . fromMaybe (-1) . M.lookup "ORE" . fst $ run
    m
    (M.fromList [("FUEL", fuel)], [])

solve2 x = show $ U.binarySearch (possible m) 1 1000000000000
    where m = buildM x

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
