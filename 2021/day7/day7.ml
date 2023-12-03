module Crabs = struct
  let from_string str = String.split_on_char ',' str |> List.map int_of_string
  let furthest_crab crabs = List.fold_left max min_int crabs

  let distances crabs =
    List.map
      (fun from_h -> List.map (fun to_h -> Int.abs (from_h - to_h)) crabs)
      crabs

  let fuel_usage calc_fuel_cost distances =
    List.map
      (fun row ->
        List.fold_left (fun acc fuel -> acc + calc_fuel_cost fuel) 0 row)
      distances

  let constant_fuel_cost fuel = fuel
  let increased_fuel_cost fuel = fuel * (1 + fuel) / 2
  let least_fuel fuel_usage = List.fold_left min max_int fuel_usage

  let solve crabs calc_fuel_cost =
    distances crabs |> fuel_usage calc_fuel_cost |> least_fuel
end

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length line > 0)

let () =
  match read_lines "day7.input" with
  | line :: [] ->
      (* part 1 *)
      (* let calc_fuel_cost = Crabs.constant_fuel_cost in *)
      (* part 2 *)
      let calc_fuel_cost = Crabs.increased_fuel_cost in
      let crabs = Crabs.from_string line in
      let fuel = Crabs.solve crabs calc_fuel_cost in
      print_endline (string_of_int fuel)
  | _ -> print_endline "?"
