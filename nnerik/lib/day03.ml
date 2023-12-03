open Core

type point = Point of int * int
type symbol = Symbol of char * point
type number = Number of int * point * point

let find_symbols input =
  input
  |> String.split_lines
  |> List.mapi ~f:(fun y line ->
         line
         |> String.to_list
         |> List.mapi ~f:(fun x c ->
                match c with
                | '.' | '0' .. '9' -> None
                | _ -> Some (Symbol (c, Point (x, y))))
         |> List.filter_opt)
  |> List.concat

let find_numbers input =
  input
  |> String.split_lines
  |> List.mapi ~f:(fun y line ->
         line
         |> String.to_list
         |> List.mapi ~f:(fun x c ->
                match c with
                | '0' .. '9' as c -> Some (c, x, y)
                | _ -> None)
         |> List.filter_opt
         |> List.group ~break:(fun (_, x1, _) (_, x2, _) -> x1 + 1 < x2)
         |> List.map ~f:(fun l ->
                let _, x1, y1 = List.hd_exn l in
                let _, x2, y2 = List.last_exn l in
                let n =
                  List.fold l ~init:0 ~f:(fun acc (c, _, _) ->
                      (acc * 10) + Char.to_int c - Char.to_int '0')
                in
                Number (n, Point (x1 - 1, y1 - 1), Point (x2 + 1, y2 + 1))))
  |> List.concat

let within (Point (x1, y1), Point (x2, y2)) (Point (x, y)) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

let part_number_value symbols (Number (n, p1, p2)) =
  match List.exists symbols ~f:(fun (Symbol (_, p)) -> within (p1, p2) p) with
  | true -> n
  | false -> 0

let solver1 input =
  let symbols = find_symbols input in
  find_numbers input
  |> List.map ~f:(part_number_value symbols)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string

let get_part_numbers numbers (Symbol (_, p)) =
  List.filter numbers ~f:(fun (Number (_, p1, p2)) -> within (p1, p2) p)

let gear_ratio = function
  | [ Number (n1, _, _); Number (n2, _, _) ] -> n1 * n2
  | _ -> 0

let solver2 input =
  let numbers = input |> find_numbers in
  input
  |> find_symbols
  |> List.filter ~f:(fun (Symbol (c, _)) -> Char.( = ) c '*')
  |> List.map ~f:(get_part_numbers numbers)
  |> List.map ~f:gear_ratio
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
