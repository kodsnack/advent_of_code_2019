module Day23
    ( solve
    )
where

import           Control.Arrow
import           Data.List
import           Data.List.Index
import           IntcodeVM
import qualified Parsing                      as P
import           Text.ParserCombinators.ReadP

parse = P.run programParser . head

type Machine = (VM, [[Int]], [(Int, [Int])])

stepOne :: Machine -> Machine
stepOne (vm, [], output) = (nvm, [], output ++ parsed)
  where
    parsed      = parsePackets nout
    (nout, nvm) = runInOutVM [-1] vm
stepOne (vm, i : is, output) = (nvm, is, output ++ parsed)
  where
    parsed      = parsePackets nout
    (nout, nvm) = runInOutVM i vm

initOne prog i = (runVM $ initVM [i] prog, [], [])

initAll :: [Int] -> [Machine]
initAll prog = map (initOne prog) [0 .. 49]

parsePackets []                  = []
parsePackets (addr : x : y : xs) = parsePackets xs ++ [(addr, [x, y])]

route :: [Machine] -> ([Machine], [(Int, [Int])])
route inputs =
    ( map popOutput $ foldl readOutput inputs inputs
    , concatMap getPackets inputs
    )
  where
    readOutput xs (_, _, []           ) = xs
    readOutput xs (_, _, (addr, o) : _) = modifyAt addr (addPacket o) xs
    addPacket packet (vm, i, o) = (vm, i ++ [packet], o)
    popOutput (vm, i, []    ) = (vm, i, [])
    popOutput (vm, i, o : xs) = (vm, i, xs)
    getPackets (_, _, []   ) = []
    getPackets (_, _, o : _) = [o]

run :: ([Machine], [(Int, [Int])]) -> ([Machine], [(Int, [Int])])
run (ms, _) = (map stepOne routed, packets) where (routed, packets) = route ms

solve1 prog =
    show
        . (!! 1)
        . snd
        . head
        . filter ((== 255) . fst)
        . concatMap snd
        $ iterate run (initial, [])
    where initial = initAll prog

run2 ms nat lastIdle lastSent = go
  where
    nms     = map stepOne routed
    wokenUp = modifyAt 0 addNat ms
    addNat (vm, is, os) = (vm, [nat], os)
    (routed, packets) = route ms
    isIdle            = all emptyQueues ms
    emptyQueues (_, [], []) = True
    emptyQueues _           = False
    checkNat = filter ((== 255) . fst) packets
    updateNat []         = nat
    updateNat [(255, a)] = a
    nNat = updateNat checkNat
    go | lastIdle && isIdle && lastSent == nat = nat !! 1
       | lastIdle && isIdle                    = run2 wokenUp nNat False nat
       | otherwise                             = run2 nms nNat isIdle lastSent

--solve2 :: [String] -> String
solve2 prog = show $ run2 initial [-1, -1] False [-1, -1]
    where initial = initAll prog

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
