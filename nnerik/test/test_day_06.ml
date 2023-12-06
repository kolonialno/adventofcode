open Core
open OUnit2

let test_part1 _ =
  let result =
    "samples/sample06.txt" |> In_channel.read_all |> Lib.Day06.solver1
  in
  assert_equal "288" result ~printer:(fun a -> a)

let test_part2 _ =
  let result =
    "samples/sample06.txt" |> In_channel.read_all |> Lib.Day06.solver2
  in
  assert_equal "71503" result ~printer:(fun a -> a)

let suite =
  "TestSuite"
  >::: [
         "test_day_06_part_1" >:: test_part1;
         "test_day_06_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
