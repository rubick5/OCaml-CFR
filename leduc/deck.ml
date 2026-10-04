type value = J | Q | K [@@deriving show]
let all_values = [J ; Q ; K]

type suit = H | S [@@deriving show]
let all_suits = [H ; S]
type card = {
  value: value;
  suit : suit;
} [@@deriving show]

let all_cards : card list =
  List.concat_map (fun value ->
    List.map (fun suit ->
      { value ; suit }
      ) all_suits
    ) all_values


let ( >: ) c1 c2 = c1.value > c2.value