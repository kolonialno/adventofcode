open Core

let solver1 input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun elf ->
         elf
         |> Str.split (Str.regexp "\n")
         |> List.fold ~f:(fun acc cal -> acc + Int.of_string cal) ~init:0)
  |> List.sort ~compare:(fun a b -> b - a)
  |> List.hd
  |> Option.value ~default:0
  |> Int.to_string

let solver2 input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun elf ->
         elf
         |> Str.split (Str.regexp "\n")
         |> List.fold ~f:(fun acc cal -> acc + Int.of_string cal) ~init:0)
  |> List.sort ~compare:(fun a b -> b - a)
  |> fun lst -> List.take lst 3 |> List.fold ~f:( + ) ~init:0 |> Int.to_string
