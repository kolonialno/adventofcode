open Core

type pick = Pick of int * int * int
type game = Game of int * pick list

let parse_pick spec =
  spec
  |> Str.split (Str.regexp ", ")
  |> List.map ~f:(fun s ->
         let n, color = Scanf.sscanf s "%u %s" (fun n c -> (n, c)) in
         match color with
         | "red" -> Pick (n, 0, 0)
         | "green" -> Pick (0, n, 0)
         | "blue" -> Pick (0, 0, n)
         | c -> failwith ("Invalid color " ^ c))
  |> List.fold
       ~init:(Pick (0, 0, 0))
       ~f:(fun (Pick (r, g, b)) (Pick (r', g', b')) ->
         Pick (r + r', g + g', b + b'))

let parse_picks specs =
  specs |> Str.split (Str.regexp "; ") |> List.map ~f:parse_pick

let parse_game line =
  match
    String.slice line 5 (String.length line) |> Str.split (Str.regexp ": ")
  with
  | [ id; picks ] -> Game (Int.of_string id, parse_picks picks)
  | _ -> failwith "Invalid input"

let id (Game (id, _)) = id

let is_possible r g b (Game (_, picks)) =
  let rec is_possible' r g b = function
    | [] -> true
    | Pick (r', g', b') :: picks ->
        if r' > r || g' > g || b' > b then false else is_possible' r g b picks
  in
  is_possible' r g b picks

let solver1 input =
  input
  |> String.split_lines
  |> List.map ~f:parse_game
  |> List.filter ~f:(is_possible 12 13 14)
  |> List.map ~f:id
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string

let power_of_minimal_set (Game (_, picks)) =
  let minimal_set =
    List.fold picks
      ~init:(Pick (0, 0, 0))
      ~f:(fun (Pick (r, g, b)) (Pick (r', g', b')) ->
        Pick (max r r', max g g', max b b'))
  in
  match minimal_set with
  | Pick (r, g, b) -> r * g * b

let solver2 input =
  input
  |> String.split_lines
  |> List.map ~f:parse_game
  |> List.map ~f:power_of_minimal_set
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
