open Core
open OUnit2

let test_part1 _ =
  let result =
    "samples/sample01a.txt" |> In_channel.read_all |> Lib.Day01.solver1
  in
  assert_equal "142" result ~printer:(fun a -> a)

let test_part2 _ =
  let result =
    "samples/sample01b.txt" |> In_channel.read_all |> Lib.Day01.solver2
  in
  assert_equal "281" result ~printer:(fun a -> a)

let suite =
  "TestSuite"
  >::: [
         "test_day_01_part_1" >:: test_part1;
         "test_day_01_part_2" >:: test_part2;
       ]

let _ = run_test_tt_main suite
