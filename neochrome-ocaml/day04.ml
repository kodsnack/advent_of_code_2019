#use "./lib.ml";;

let rec digits_of num =
  if num > 9 then digits_of (num / 10) @ [num mod 10]
  else [num]

let rec never_decreasing = function
  | [] | [_] -> true
  | d1 :: (d2 :: _ as digits) ->
    if d1 > d2 then false else never_decreasing digits

let groups_of_same = List.(chunk (fun d -> d) >> map (snd >> length))

let () =
  let first,last = (284639,748759) in
  Range.fold first last (fun (part1,part2) num ->
    let digits = digits_of num in
    let groups = groups_of_same digits in
    let has_group_of_2 = List.exists (fun len -> len > 1) groups in
    let has_group_of_max_2 = List.exists (fun len -> len = 2) groups in

    if never_decreasing digits then
      (part1 + if has_group_of_2 then 1 else 0),
      (part2 + if has_group_of_max_2 then 1 else 0)
    else
      part1,part2
  ) (0,0)
  |> fun (part1,part2) ->
    Printf.printf "part1: %d\npart2: %d\n" part1 part2
