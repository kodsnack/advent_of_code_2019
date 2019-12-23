module Day22
    ( solve
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as S
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP
import qualified Utils                        as U

data Deal = IntoNew | WithIncrement Integer | Cut Integer | Combined Integer Integer deriving Show

dealEmpty = Combined 1 0
dealAppend m (Combined k c) IntoNew             = Combined (-k) ((-1) - c)
dealAppend m (Combined k c) (WithIncrement inc) = Combined
    ((inv * k) `mod` m)
    ((inv * c) `mod` m)
    where inv = powMod m inc (totient m - 1) `mod` m
dealAppend m (Combined k c) (Cut cut) = Combined k (c + cut)
dealAppend m a b = dealAppend m (dealAppend m dealEmpty a) b

foldDeals dSize = foldl (dealAppend dSize) dealEmpty . reverse

intoNew = string "deal into new stack" >> return IntoNew
withIncrement =
    WithIncrement
        <$> (string "deal with increment " >>= const (P.integerAnd' eof))
cut = Cut <$> (string "cut " >>= const (P.integerAnd' eof))

deal = choice [intoNew, withIncrement, cut]


powMod _ _ 0 = 1
powMod m x 1 = x `mod` m
powMod m x n | even n    = powMod m modSquare (n `div` 2)
             | otherwise = (x * powMod m modSquare ((n - 1) `div` 2)) `mod` m
    where modSquare = x * (x `mod` m)

newIndex dSize i IntoNew = (dSize - 1) - i `mod` dSize
newIndex dSize i (WithIncrement inc) =
    (i * powMod dSize inc (totient dSize - 1)) `mod` dSize
newIndex dSize i (Cut c       ) = (i + c) `mod` dSize
newIndex dSize i (Combined k x) = (i * k + x) `mod` dSize

powerDeals dSize pow i (Combined k x) =
    (powMod dSize k pow * i + x * geoSum dSize k pow) `mod` dSize

geoSum m k n = (1 - powMod m k n) * powMod m (1 - k `mod` m) (totient m - 1)

newIndex' dSize i IntoNew             = (dSize - 1) - i
newIndex' dSize i (WithIncrement inc) = (i * inc) `mod` dSize
newIndex' dSize i (Cut           c  ) = (i - c) `mod` dSize

shuffle dSize deals i = foldl (newIndex dSize) i (reverse deals)

shufflePow pow dSize deals i = powerDeals dSize pow i (foldDeals dSize deals)

shuffle' dSize deals i = foldl (newIndex' dSize) i deals

parse = map (P.run deal)


decksize1 = 10007

decksize2 = 119315717514047

totient 10              = 4
totient 10007           = 10006
totient 119315717514047 = 119315717514046

nShuffles = 101741582076661

solve1 xs = show $ shuffle' decksize1 xs 2019

solve2 xs = show $ shufflePow nShuffles decksize2 xs 2020

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
