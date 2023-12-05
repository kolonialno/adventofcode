open Core

(* Part 1 *)
let parse_seeds input =
  input |> String.split ~on:' ' |> List.tl_exn |> List.map ~f:Int.of_string

let parse_submap input =
  match input |> String.split ~on:' ' |> List.map ~f:Int.of_string with
  | [ dst; src; len ] -> (src, src + len, dst - src)
  | _ -> failwith "invalid map"

let parse_maps input =
  input |> String.split_lines |> List.tl_exn |> List.map ~f:parse_submap

let parse_input input =
  let sections = Str.split (Str.regexp "\n\n") input in
  ( parse_seeds (List.hd_exn sections),
    List.map ~f:parse_maps (List.tl_exn sections) )

let rec apply_map input = function
  | [] -> input
  | (start, end_, offset) :: rest ->
      if start <= input && input < end_ then input + offset
      else apply_map input rest

let solver1 input =
  let seeds, maps = parse_input input in
  List.fold seeds ~init:Int.max_value ~f:(fun acc seed ->
      min acc (List.fold maps ~init:seed ~f:apply_map))
  |> Int.to_string

(* Part 2 *)
let rec apply_map_to_range submaps (start, end_) =
  if start >= end_ then []
  else
    match submaps with
    | [] -> [ (start, end_) ]
    | (start', end', offset) :: rest ->
        (offset + max start start', offset + min end_ end')
        :: List.concat
             [
               apply_map_to_range rest (start, min end_ start');
               apply_map_to_range rest (max start end', end_);
             ]

let rec ranges = function
  | s :: n :: rest -> (s, s + n) :: ranges rest
  | _ -> []

let solver2 input =
  let seeds, maps = parse_input input in
  List.fold maps ~init:(ranges seeds) ~f:(fun acc map ->
      List.concat_map acc ~f:(apply_map_to_range map))
  |> List.fold ~init:Int.max_value ~f:(fun acc (start, _) -> min acc start)
  |> Int.to_string
