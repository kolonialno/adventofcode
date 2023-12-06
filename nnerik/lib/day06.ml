open Core

(* Part 1 *)

let parse_line line =
  line
  |> Str.split (Str.regexp " +")
  |> List.tl_exn
  |> List.map ~f:Int.of_string

let parse_input input =
  match input |> String.split_lines with
  | [ t; d ] -> (parse_line t, parse_line d)
  | _ -> failwith "Invalid input"

let count_wins t d =
  let t' = Float.of_int t in
  let d' = Float.of_int d in
  let m = Int.of_float ((t' -. sqrt ((t' *. t') -. (4. *. d'))) /. 2.) in
  t - 1 - (2 * m)

let solver1 input =
  let time, distance = parse_input input in
  List.map2_exn time distance ~f:count_wins
  |> List.fold ~init:1 ~f:( * )
  |> Int.to_string

(* Part 2 *)
let parse_line_2 line =
  line
  |> Str.split (Str.regexp " +")
  |> List.tl_exn
  |> List.fold ~init:"" ~f:( ^ )
  |> Int.of_string

let parse_input_2 input =
  match input |> String.split_lines with
  | [ t; d ] -> (parse_line_2 t, parse_line_2 d)
  | _ -> failwith "Invalid input"

let solver2 input =
  let time, distance = parse_input_2 input in
  count_wins time distance |> Int.to_string
