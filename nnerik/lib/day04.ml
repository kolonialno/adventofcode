open Core

let parse_input input =
  input
  |> String.split_lines
  |> List.map ~f:(Str.split (Str.regexp {|: *\| *| *|}))
  |> List.map ~f:(fun l ->
         match l with
         | [ _; w; n ] ->
             ( w |> Str.split (Str.regexp " +") |> Set.of_list (module String),
               n |> Str.split (Str.regexp " +") |> Set.of_list (module String)
             )
         | _ -> failwith "Invalid input")

let wins (winners, numbers) = Set.inter winners numbers |> Set.length

let calculate_points cards =
  cards |> wins |> fun n -> if n = 0 then 0 else Int.( ** ) 2 (n - 1)

let solver1 input =
  input
  |> parse_input
  |> List.map ~f:calculate_points
  |> List.fold ~init:0 ~f:Int.( + )
  |> Int.to_string

let rec claim n m = function
  | [] -> []
  | x :: xs -> if n > 0 then (x + m) :: claim (n - 1) m xs else x :: xs

let solver2 input =
  let cards = parse_input input in
  let count = List.init (List.length cards) ~f:(const 1) in
  let rec aux score cards' = function
    | [] -> score
    | n :: ns ->
        aux (score + n) (List.drop cards' 1)
          (claim (wins (List.hd_exn cards')) n ns)
  in
  aux 0 cards count |> Int.to_string
