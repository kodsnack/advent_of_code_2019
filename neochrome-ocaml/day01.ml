#use "./lib.ml";;

let fuel_for_mass mass = mass / 3 - 2

let _examples_part1 =
  let () = assert (12 |> fuel_for_mass = 2) in
  let () = assert (14 |> fuel_for_mass = 2) in
  let () = assert (1969 |> fuel_for_mass = 654) in
  let () = assert (100756 |> fuel_for_mass = 33583) in
  ()

let fuel_for_fuel =
  let rec calc total mass =
    let fuel_mass = fuel_for_mass mass in
    if fuel_mass < 1 then total
    else calc (total + fuel_mass) fuel_mass
  in
  calc 0

let _examples_part2 =
  let () = assert (12 |> fuel_for_fuel = 2) in
  let () = assert (14 |> fuel_for_fuel = 2) in
  let () = assert (1969 |> fuel_for_fuel = 966) in
  let () = assert (100756 |> fuel_for_fuel = 50346) in
  ()

let () =
  File.open_in "./day01.input" (fun ch ->
    let masses = Seq.of_lines ch |> Seq.map int_of_string |> List.of_seq in
    let () =
      masses
      |> List.map fuel_for_mass
      |> List.fold_left (+) 0
      |> Printf.printf "part1: %d\n%!"
    in
    let () =
      masses
      |> List.map fuel_for_fuel
      |> List.fold_left (+) 0
      |> Printf.printf "part2: %d\n%!"
    in
    ()
  )
