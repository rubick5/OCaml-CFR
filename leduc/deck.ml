type value = J | Q | K [@@deriving show]
type suit = H | S [@@deriving show]
type card = {
value: value;
suit : suit;
} [@@deriving show]