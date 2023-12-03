module Coord = struct
  type coord = { x : int; y : int } [@@deriving show]

  let rec make_horizontal_line y start finish acc =
    if start > finish then acc
    else
      let coord = { x = start; y } in
      make_horizontal_line y (start + 1) finish (coord :: acc)

  let make_horizontal_line y start finish = make_horizontal_line y start finish

  let rec make_vertical_line x start finish acc =
    if start > finish then acc
    else
      let coord = { x; y = start } in
      make_vertical_line x (start + 1) finish (coord :: acc)

  let make_vertical_line x start finish = make_vertical_line x start finish

  let rec make_diagonal_line start_x start_y finish_x finish_y acc =
    (* Assumes that start_x is on the left side of finish_x *)
    if start_x > finish_x then acc
    else
      let coord = { x = start_x; y = start_y } in
      let new_x = start_x + 1 in
      let new_y = if start_y < finish_y then start_y + 1 else start_y - 1 in
      make_diagonal_line new_x new_y finish_x finish_y (coord :: acc)
end

module CoordState = Map.Make (struct
  type t = Coord.coord

  let compare = compare
end)

module Vents = struct
  let inc_count state =
    match state with None -> Some 1 | Some s -> Some (s + 1)

  let rec upsert vents coords =
    match coords with
    | [] -> vents
    | head :: tail ->
        let vents = CoordState.update head inc_count vents in
        upsert vents tail

  let show_vent vent =
    let coord, count = vent in
    Coord.show_coord coord ^ string_of_int count

  let show_vents vents =
    CoordState.fold
      (fun coord count acc -> show_vent (coord, count) :: acc)
      vents []

  let empty = CoordState.empty

  let count_overlaps vents =
    CoordState.fold
      (fun _ count acc -> if count > 1 then acc + 1 else acc)
      vents 0
end

module Command = struct
  type command = {
    start_x : int;
    start_y : int;
    finish_x : int;
    finish_y : int;
  }
  [@@deriving show]

  let re = Re2.create_exn "[0-9]+"

  let parse line =
    match Re2.find_all_exn re line |> List.map int_of_string with
    | [ left_x; left_y; right_x; right_y ] ->
        let start_x, start_y, finish_x, finish_y =
          if left_x == right_x && left_y > right_y then
            (right_x, right_y, left_x, left_y)
          else if left_x <= right_x then (left_x, left_y, right_x, right_y)
          else (right_x, right_y, left_x, left_y)
        in
        Ok { start_x; start_y; finish_x; finish_y }
    | _ -> Error "Not a valid coords line"

  let rec parse_lines lines acc =
    match lines with
    | [] -> Ok (List.rev acc)
    | line :: lines -> (
        match parse line with
        | Ok cmd -> parse_lines lines (cmd :: acc)
        | Error e -> Error e)

  let parse_lines lines = parse_lines lines []

  let make_coords cmd =
    if cmd.start_x == cmd.finish_x then
      (* let _ = print_endline ("vert " ^ show_command cmd) in *)
      Coord.make_vertical_line cmd.start_x cmd.start_y cmd.finish_y []
    else if cmd.start_y == cmd.finish_y then
      (* let _ = print_endline ("hor " ^ show_command cmd) in *)
      Coord.make_horizontal_line cmd.start_y cmd.start_x cmd.finish_x []
    else if
      Int.abs (cmd.start_x - cmd.finish_x)
      == Int.abs (cmd.start_y - cmd.finish_y)
    then
      (* let _ = print_endline ("diag " ^ show_command cmd) in *)
      Coord.make_diagonal_line cmd.start_x cmd.start_y cmd.finish_x cmd.finish_y
        []
    else []
end

let read_lines file =
  In_channel.with_open_bin file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length line > 0)

let rec apply commands vents =
  match commands with
  | [] -> vents
  | command :: commands ->
      let vents = Command.make_coords command |> Vents.upsert vents in
      apply commands vents

let () =
  let lines = read_lines "day5.input" in
  match Command.parse_lines lines with
  | Ok commands ->
      let vents = apply commands Vents.empty in
      let overlap_count = Vents.count_overlaps vents in
      print_endline (string_of_int overlap_count)
      (* let _ = print_endline (string_of_int overlap_count) in *)
      (* List.iter print_endline (Vents.show_vents vents) *)
      (* let _ = print_endline (string_of_int overlap_count) in *)
      (* let xs = Coord.make_diagonal_line 7 9 9 7 [] in *)
      (* List.iter (fun c -> print_string (Coord.show_coord c)) xs *)
  | Error e -> print_endline e
