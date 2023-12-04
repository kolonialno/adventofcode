open Core
open OUnit2

let test_part1 _ =
  let result =
    "samples/sample04.txt" |> In_channel.read_all |> Lib.Day04.solver1
  in
  assert_equal "13" result ~printer:(fun a -> a)

let test_part2 _ =
  let result =
    "samples/sample04.txt" |> In_channel.read_all |> Lib.Day04.solver2
  in
  assert_equal "30" result ~printer:(fun a -> a)

let suite =
  "TestSuite"
  >::: [
         "test_day_04_part_1" >:: test_part1;
         "test_day_04_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
