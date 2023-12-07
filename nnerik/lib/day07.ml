open Core

type hand = Hand of int list
type hand_bid = HandBid of hand * int

let card_value j_value = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> j_value
  | 'T' -> 10
  | c -> Char.to_int c - Char.to_int '0'

let parse_input value_function input =
  input
  |> String.split_lines
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(fun hand_bid ->
         match hand_bid with
         | [ hand; bid ] ->
             HandBid
               ( Hand (hand |> String.to_list |> List.map ~f:value_function),
                 Int.of_string bid )
         | _ -> failwith "Invalid input")

let type_of_hand hand =
  let hand' = List.filter hand ~f:(fun x -> x > 1) in
  match
    hand'
    |> List.sort ~compare:Int.compare
    |> List.group ~break:(fun a b -> a < b)
    |> List.map ~f:List.length
    |> List.sort ~compare:Int.descending
  with
  | [] -> [ 5 ]
  | hd :: rest -> (hd + 5 - List.length hand') :: rest

let compare_hands (HandBid (Hand hand1, _)) (HandBid (Hand hand2, _)) =
  let cmp =
    List.compare Int.compare (type_of_hand hand1) (type_of_hand hand2)
  in
  if cmp = 0 then List.compare Int.compare hand1 hand2 else cmp

let bid = function
  | HandBid (_, bid) -> bid

let solver value_function input =
  input
  |> parse_input value_function
  |> List.sort ~compare:compare_hands
  |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * bid hand))
  |> Int.to_string

let solver1 = solver (card_value 11)
let solver2 = solver (card_value 1)
