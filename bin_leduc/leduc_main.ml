open Leduc.Game

let g : game_state = {
  a = 5;
  b = 10;
}

let () = print_endline (show_game_state g)