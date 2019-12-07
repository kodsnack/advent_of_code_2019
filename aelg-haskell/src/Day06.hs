module Day06
    ( solve
    )
where

import           Control.Arrow
import qualified Data.Map.Strict               as M
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing                       as P

parseOrbit = P.run $ do
    parent <- many1 . satisfy $ isAlphaNum
    char ')'
    child <- many1 . satisfy $ isAlphaNum
    eof
    return (child, parent)

buildOT = M.fromList

countParents count m a = go (count + 1) (M.lookup a m)
    where go count = maybe count (countParents count m)

countOrbits m = M.foldr' go 0 m where go a b = b + countParents 0 m a

parse = map parseOrbit

solve1 = show . countOrbits . buildOT

parents xs ot a = maybe xs (go xs) (M.lookup a ot)
    where go xs a = parents (a : xs) ot a

firstCommonParent parent (a : as) (b : bs) | a == b = firstCommonParent a as bs
                                           | otherwise = parent

countNonCommon common = length . drop 1 . dropWhile (common /=)

solve2 orbits = show $ numParentsYou + numParentsSan
  where
    numParentsYou = countNonCommon commonParent parentsYou
    numParentsSan = countNonCommon commonParent parentsSan
    commonParent  = firstCommonParent "no common" parentsYou parentsSan
    parentsYou    = parents [] ot "YOU"
    parentsSan    = parents [] ot "SAN"
    ot            = buildOT orbits

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
