open Core
open OUnit

let test_part1 () =
  let result =
    "samples/sample01.txt" |> In_channel.read_all |> Lib.Day02.solver1
  in
  assert_equal result "TODO"

let test_part2 () =
  let result =
    "samples/sample01.txt" |> In_channel.read_all |> Lib.Day02.solver2
  in
  assert_equal result "TODO"

let suite =
  "TestSuite"
  >::: [
         "test_day_02_part_1" >:: test_part1;
         "test_day_02_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
