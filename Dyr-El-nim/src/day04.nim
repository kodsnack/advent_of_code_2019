import re

let
    doublePattern = re"(\d)\1"
    longPattern = re"(\d)\1{2,}"

proc checkDouble(s: string): bool =
    result = s.contains(doublePattern)

proc filterLong(s: string): string =
    result = s.replace(longPattern)

proc checkOrder(s: string): bool =
    var
        lastc = ' '
    for c in s:
        if c < lastc:
            return false
        lastc = c
    return true

proc test1*(i: int): bool =
    let
        s = $i
    result = checkOrder(s) and checkDouble(s)

proc test2*(i: int): bool =
    let
        s = $i
    result = checkOrder(s) and checkDouble(filterLong(s))

proc countPasswords*(lo, hi:int, pswdChk: proc (i: int):bool): int =
    for i in lo..hi:
        if pswdChk(i):
            result.inc