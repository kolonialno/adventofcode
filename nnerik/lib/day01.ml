open Core

let rec first_digit = function
  | [] -> failwith "No digit found"
  | char :: rest ->
      if Char.is_digit char then Char.to_int char - 48 else first_digit rest

let solver1 input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
         let first = String.to_list line |> first_digit in
         let last = String.to_list_rev line |> first_digit in
         (first * 10) + last)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string

let rec first_alpha_digit pos prefixes str =
  if String.length str <= pos then failwith "No digit found"
  else
    let char = String.get str pos in
    if Char.is_digit char then Char.to_int char - 48
    else
      match
        List.find_mapi prefixes ~f:(fun i prefix ->
            if
              String.is_prefix
                (String.slice str pos (String.length str))
                ~prefix
            then Some i
            else None)
      with
      | Some i -> i + 1
      | None -> first_alpha_digit (pos + 1) prefixes str

let literals =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let reverse_literals = List.map literals ~f:String.rev

let solver2 input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
         let first = line |> first_alpha_digit 0 literals in
         let last =
           line |> String.rev |> first_alpha_digit 0 reverse_literals
         in
         (first * 10) + last)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
