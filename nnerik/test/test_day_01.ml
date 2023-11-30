(* open Core *)
open OUnit

let test_part1 () =
  (* let result =
    "samples/sample01.txt" |> In_channel.read_all |> Lib.Day01.solver1
  in *)
  let result = "" in
  assert_equal result ""

let test_part2 () =
  (* let result =
    "samples/sample01.txt" |> In_channel.read_all |> Lib.Day01.solver2
  in *)
  let result = "" in
  assert_equal result ""

let suite =
  "TestSuite"
  >::: [
         "test_day_01_part_1" >:: test_part1;
         "test_day_01_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
