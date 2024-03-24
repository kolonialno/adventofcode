open Core
open Lib.Solver

let usage_msg = "advent [-1] [-2] <filename>"
let input_file = ref ""
let part_1 = ref false
let part_2 = ref false

let speclist =
  [ ("-1", Arg.Set part_1, "Run part 1"); ("-2", Arg.Set part_2, "Run part 2") ]

let () = Arg.parse speclist (fun s -> input_file := s) usage_msg

(* Extract the day number from the filename *)
let day =
  if String.equal !input_file "" then (
    Stdio.print_endline "No input file specified!";
    exit 0);
  try
    let _ = Str.search_forward (Str.regexp {|[0-9]+|}) !input_file 0 in
    Str.matched_group 0 !input_file
  with Stdlib.Not_found ->
    Stdio.print_endline "File name must conatin a day number!";
    exit 0

(* Get the part number from arguments *)
let part =
  if not (!part_1 || !part_2) then (
    Stdio.print_endline "No part specified!";
    exit 0);
  if !part_1 && !part_2 then (
    Stdio.print_endline "Too many parts specified!";
    exit 0);
  if !part_1 then 1 else 2

(* Get the solver function *)
let solver =
  let (module S) : (module SolverType) =
    match day with
    | "01" -> (module Lib.Day01)
    | "02" -> (module Lib.Day02)
    | "03" -> (module Lib.Day03)
    | "04" -> (module Lib.Day04)
    | "05" -> (module Lib.Day05)
    | "06" -> (module Lib.Day06)
    | "07" -> (module Lib.Day07)
    | _ -> (module Lib.Dummy_solver)
  in
  match part with
  | 1 -> S.solver1
  | _ -> S.solver2

let () =
  Stdio.print_endline ("Day " ^ day ^ " part " ^ Int.to_string part);
  let input = In_channel.read_all !input_file in
  let start_time = Time_float.now () in
  let result = solver input in
  let end_time = Time_float.now () in
  Stdio.print_endline result;
  Stdio.print_endline
    (Printf.sprintf "Time: %7.2f ms"
       (Time_float.Span.to_ms (Time_float.diff end_time start_time)))
