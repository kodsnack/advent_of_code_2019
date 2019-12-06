#use "./lib.ml";;

let parse_segment segment = Scanf.sscanf segment "%c%d" (fun dir dist -> (dir,dist))

let parse path =
  path
  |> String.split_on_char ','
  |> List.map parse_segment

module IntPair = struct
  type t = int * int
  let compare (ax,ay) (bx,by) =
    match Int.compare ax bx with
    | 0 -> Int.compare ay by
    | n -> n
end
module IntPairSet = Set.Make(IntPair)
module IntPairMap = Map.Make(IntPair)

let follow_segment (x,y) (dir,dist) =
  match dir with
  | 'U' -> (x,y+dist), List.init dist (fun i -> (x,y+i))
  | 'R' -> (x+dist,y), List.init dist (fun i -> (x+i,y))
  | 'D' -> (x,y-dist), List.init dist (fun i -> (x,y-i))
  | 'L' -> (x-dist,y), List.init dist (fun i -> (x-i,y))
  | _ -> failwith "bad direction"

let follow path =
  List.fold_left (fun (pos,steps,locations) segment ->
    let pos',visited = follow_segment pos segment in
    let steps',locations' = List.fold_left
      (fun (steps,locations) pos ->
        steps+1,IntPairMap.update pos (function
          | None -> Some steps
          | keep -> keep
        ) locations
      ) (steps,locations) visited
    in
    pos',steps',locations'
  ) ((0,0),0,IntPairMap.empty) path
  |> fun (_,_,locations) -> locations

let distance (x,y) = (abs x) + (abs y)

let part1 wire1_path wire2_path =
  let wire1 = parse wire1_path |> follow |> IntPairMap.keys in
  let wire2 = parse wire2_path |> follow |> IntPairMap.keys in
  IntPairSet.(
    inter (of_list wire1) (of_list wire2)
    |> remove (0,0)
    |> elements
    |> List.map distance
    |> List.min
  )

let _part1_examples =
  assert (part1 "R8,U5,L5,D3" "U7,R6,D4,L4" = 6);
  assert (part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" = 159);
  assert (part1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" = 135)

let part2 wire1_path wire2_path =
  let wire1 = parse wire1_path |> follow in
  let wire2 = parse wire2_path |> follow in
  IntPairMap.(
    merge (fun pos s1 s2 ->
      if pos = (0,0) then None
      else
        match s1,s2 with
        | None,None -> None
        | Some _,None -> None
        | None,Some _ -> None
        | Some s1,Some s2 -> Some (s1 + s2)
    ) wire1 wire2
    |> bindings
    |> List.map snd
    |> List.min
  )

let _part2_examples =
  assert (part2 "R8,U5,L5,D3" "U7,R6,D4,L4" = 30);
  assert (part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" = 610);
  assert (part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" = 410)

let () =
  File.open_in "./day03.input" (fun ch ->
    let wire1,wire2 =
      Seq.of_lines ch
      |> List.of_seq
      |> function
        | [w1;w2] -> w1,w2
        | _ -> failwith "bad input"
    in
    let () = part1 wire1 wire2 |> Printf.printf "part1: %d\n%!" in
    let () = part2 wire1 wire2 |> Printf.printf "part2: %d\n%!" in
    ()
  )
