open Core
open OUnit2

let test_part1 _ =
  let result =
    "samples/sample05.txt" |> In_channel.read_all |> Lib.Day05.solver1
  in
  assert_equal "35" result ~printer:(fun a -> a)

let test_part2 _ =
  let result =
    "samples/sample05.txt" |> In_channel.read_all |> Lib.Day05.solver2
  in
  assert_equal "46" result ~printer:(fun a -> a)

let suite =
  "TestSuite"
  >::: [
         "test_day_05_part_1" >:: test_part1;
         "test_day_05_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
