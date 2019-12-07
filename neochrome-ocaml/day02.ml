#use "./lib.ml";;

module Intcode = struct
  let execute input =
    let mem = Array.copy input in
    let read mode addr = Array.get mem (mode addr) in
    let write addr value = Array.set mem addr value in
    let abs addr = addr in

    let rec step pc =
      let op_code = mem.(pc) in
      if op_code = 99 then mem
      else
        let arg1 = mem.(pc + 1) in
        let arg2 = mem.(pc + 2) in
        let dest = mem.(pc + 3) in
        let pc' = pc + 4 in
        match op_code with
        | 1 -> (read abs arg1) + (read abs arg2) |> write dest; step pc'
        | 2 -> (read abs arg1) * (read abs arg2) |> write dest; step pc'
        | x -> failwith (Printf.sprintf "invalid opcode: %d" x)
    in
    step 0
end

let _examples_part1 =
  let () = assert ([|1;0;0;0;99|] |> Intcode.execute = [|2;0;0;0;99|]) in
  let () = assert ([|2;3;0;3;99|] |> Intcode.execute = [|2;3;0;6;99|]) in
  let () = assert ([|2;4;4;5;99;0|] |> Intcode.execute = [|2;4;4;5;99;9801|]) in
  let () = assert ([|1;1;1;4;99;5;6;0;99|] |> Intcode.execute = [|30;1;1;4;2;5;6;0;99|]) in
  ()

let run input noun verb =
  let input' = Array.copy input in
  let () = input'.(1) <- noun
  and () = input'.(2) <- verb
  in Intcode.execute input'

let rec search input noun verb =
  let res = run input noun verb in
  if res.(0) = 19690720 then (noun,verb)
  else
    match (noun,verb) with
    | 99,99 -> failwith "not able to find solution"
    | n,99 -> search input (n+1) 0
    | n,v -> search input n (v+1)

let () =
  File.open_in "./day02.input" (fun ch ->
    let input =
      Seq.of_lines ch
      |> Seq.map (String.split_on_char ',')
      |> List.of_seq
      |> List.flatten
      |> List.map int_of_string
      |> Array.of_list
    in
    let () =
      run input 12 02 |> fun res -> Printf.printf "part1: %d\n%!" res.(0)
    in
    let () =
      search input 0 0 |> fun (noun,verb) -> Printf.printf "part2: 100 * %d + %d = %d\n%!" noun verb (100*noun+verb)
    in
    ()
  )
