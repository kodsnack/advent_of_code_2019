module Day04Ugly

open System.Text.RegularExpressions

let input = [246540..787419]

let isIncreasing x =
    let [|a;b;c;d;e;f|] = (x |> string).ToCharArray()
    (a < b || a = b) &&
    (b < c || b = c) &&
    (c < d || c = d) &&
    (d < e || d = e) &&
    (e < f || e = f)

module Part1 =
    let hasTwins (x:int) = Regex.IsMatch(x |> string, @"(\d)\1")

    input
    |> List.filter hasTwins
    |> List.filter isIncreasing
    |> List.length
    |> printfn "%O"

module Part2DisgustingRegex =
    let hasNonClusterTwins (x:int) =
        Regex.IsMatch(x |> string,
            @"((([^1)]|^)11([^1]|$))|(([^2)]|^)22([^2]|$))|(([^3)]|^)33([^3]|$))|(([^4)]|^)44([^4]|$))|(([^5)]|^)55([^5]|$))|(([^6)]|^)66([^6]|$))|(([^7)]|^)77([^7]|$))|(([^8)]|^)88([^8]|$))|(([^9)]|^)99([^9]|$)))")

    input
    |> List.filter hasNonClusterTwins
    |> List.filter isIncreasing
    |> List.length
    |> printfn "%O"
