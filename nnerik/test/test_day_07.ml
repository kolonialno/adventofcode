open Core
open OUnit2

let test_part1 _ =
  let result =
    "samples/sample07.txt" |> In_channel.read_all |> Lib.Day07.solver1
  in
  assert_equal "6440" result ~printer:(fun a -> a)

let test_part2 _ =
  let result =
    "samples/sample07.txt" |> In_channel.read_all |> Lib.Day07.solver2
  in
  assert_equal "5905" result ~printer:(fun a -> a)

let suite =
  "TestSuite"
  >::: [
         "test_day_07_part_1" >:: test_part1;
         "test_day_07_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
