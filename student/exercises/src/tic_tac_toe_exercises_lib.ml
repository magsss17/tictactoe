open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game_ttt =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let empty_game_omok =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Omok in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let win_for_o_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
;;

let diag_win_for_o_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let invalid_state_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let tie_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
;;

let win_for_x_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 4 }
;;

let non_win_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 13; column = 9 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 8 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 13; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 12 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 10; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 9; column = 9 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 8 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 6; column = 7 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 8; column = 12 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 18; column = 13 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 14 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 15; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 11; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 12; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 8; column = 14 }
;;

let minor_diag_win_for_x_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 14; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 12; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 12; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 11; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 10; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

let win_for_o_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

let invalid_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let dimension = match game_kind with Tic_tac_toe -> 3 | Omok -> 15 in
  let l = List.init dimension ~f:(fun i -> i) in
  let all_pos =
    List.concat
      (List.map l ~f:(fun x ->
         List.map l ~f:(fun y -> { Position.row = x; column = y })))
  in
  let piece_pos = Map.keys pieces in
  Set.to_list
    (Set.diff
       (Set.of_list (module Position) all_pos)
       (Set.of_list (module Position) piece_pos))
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(* directions: 0 - up, 1 - up right, 2 - right, 3 - right down, 4 - down, 5 -
   down left, 6 - left, 7 - left up*)
(*let check_neighbor_exists ~(direction : int) ~(pos : Position.t)
  ~(positions : Piece.t Position.Map.t) : bool = let rec helper (i : int)
  offsets : bool = match i, offsets with | 0, hd :: _ -> Map.exists positions
  ~f:(hd po) | _, _ :: tl -> helper (i - 1) tl | _, _ -> false in helper
  direction (Position.all_offsets pos) ;;*)

let rec add_neighbors
  ~(pos_list : (Position.t * int) list)
  ~(p : Piece.t)
  ~(pieces : Piece.t Position.Map.t)
  : (Position.t * int) list
  =
  match pos_list with
  | [] -> []
  | (fst, snd) :: tl ->
    let next_piece = (List.nth_exn Position.all_offsets snd) fst in
    (match Map.find pieces next_piece with
     | None -> add_neighbors ~pos_list:tl ~p ~pieces
     | Some s ->
       let rest = add_neighbors ~pos_list:tl ~p ~pieces in
       if Piece.equal s p then (next_piece, snd) :: rest else rest)
;;

let reduce_list
  ~(pos_list : (Position.t * int) list)
  ~(p : Piece.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(times : int)
  : (Position.t * int) list
  =
  let rec helper (index : int) (l : (Position.t * int) list)
    : (Position.t * int) list
    =
    match index with
    | 1 -> l
    | _ -> helper (index - 1) (add_neighbors ~pos_list:l ~p ~pieces)
  in
  helper times pos_list
;;

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  (* TODO: 1. populate initial position list for x and o 2. pass it through
     reduce list 3. check if returned list length > 0

     options: 1. one of the returned list has length > 0 --> that player wins
     2. no one wins but no more available moves --> tie 3. game in
     progress *)
  let times = match game_kind with Tic_tac_toe -> 3 | Omok -> 5 in
  let l = List.init 4 ~f:(fun i -> i) in
  let x_pieces = Map.filter pieces ~f:(fun p -> Piece.equal p Piece.X) in
  let o_pieces = Map.filter pieces ~f:(fun p -> Piece.equal p Piece.O) in
  let init_x = List.cartesian_product (Map.keys x_pieces) l in
  let init_o = List.cartesian_product (Map.keys o_pieces) l in
  let num_x =
    List.length (reduce_list ~pos_list:init_x ~p:Piece.X ~pieces ~times)
  in
  let num_o =
    List.length (reduce_list ~pos_list:init_o ~p:Piece.O ~pieces ~times)
  in
  match num_x, num_o with
  | 0, 0 ->
    if List.is_empty (available_moves ~game_kind ~pieces)
    then Game_over { winner = None }
    else Game_continues
  | _, 0 -> Game_over { winner = Some Piece.X }
  | 0, _ -> Game_over { winner = Some Piece.O }
  | _, _ ->
    if List.is_empty (available_moves ~game_kind ~pieces)
    then Game_over { winner = None }
    else Illegal_state
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let my_pieces = Map.filter pieces ~f:(fun p -> Piece.equal p me) in
  let available = available_moves ~game_kind ~pieces in
  List.filter available ~f:(fun x ->
    match
      evaluate ~game_kind ~pieces:(Map.set my_pieces ~key:x ~data:me)
    with
    | Game_over _ -> true
    | _ -> false)
;;

(* Exercise 4. MODIFIED - returns list of moves that you must play to prevent
   losing *)
let blocking_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  winning_moves ~me:(Piece.flip me) ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x_ttt.game_kind
           ~pieces:win_for_x_ttt.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves
           ~game_kind:non_win_ttt.game_kind
           ~pieces:non_win_ttt.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate
           ~game_kind:win_for_x_ttt.game_kind
           ~pieces:win_for_x_ttt.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win_ttt.game_kind
           ~pieces:non_win_ttt.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         blocking_moves
           ~me:piece
           ~game_kind:non_win_ttt.game_kind
           ~pieces:non_win_ttt.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x_ttt);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win_ttt);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {| 
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves
      ~game_kind:win_for_x_ttt.game_kind
      ~pieces:win_for_x_ttt.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evaluate_win_for_x_ttt" =
  print_endline
    (evaluate ~game_kind:win_for_x_ttt.game_kind ~pieces:win_for_x_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_non_win_ttt" =
  print_endline
    (evaluate ~game_kind:non_win_ttt.game_kind ~pieces:non_win_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

let%expect_test "evaluate_win_for_o_ttt" =
  print_endline
    (evaluate ~game_kind:win_for_o_ttt.game_kind ~pieces:win_for_o_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_tie_ttt" =
  print_endline
    (evaluate ~game_kind:tie_ttt.game_kind ~pieces:tie_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner())) |}]
;;

let%expect_test "evaluate_diag_win_o_ttt" =
  print_endline
    (evaluate
       ~game_kind:diag_win_for_o_ttt.game_kind
       ~pieces:diag_win_for_o_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_invalid_game_ttt" =
  print_endline
    (evaluate
       ~game_kind:invalid_state_ttt.game_kind
       ~pieces:invalid_state_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner())) |}]
;;

let%expect_test "evaluate_win_for_x_omok" =
  print_endline
    (evaluate
       ~game_kind:win_for_x_omok.game_kind
       ~pieces:win_for_x_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_non_win_omok" =
  print_endline
    (evaluate ~game_kind:non_win_omok.game_kind ~pieces:non_win_omok.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

let%expect_test "evaluate_minor_diag_win_for_x_omok" =
  print_endline
    (evaluate
       ~game_kind:minor_diag_win_for_x_omok.game_kind
       ~pieces:minor_diag_win_for_x_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_win_for_o_omok" =
  print_endline
    (evaluate
       ~game_kind:win_for_o_omok.game_kind
       ~pieces:win_for_o_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_invalid_omok" =
  print_endline
    (evaluate ~game_kind:invalid_omok.game_kind ~pieces:invalid_omok.pieces
     |> Evaluation.to_string);
  [%expect {| Illegal_state |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test!*)
let%expect_test "winning_move_ttt" =
  let positions =
    winning_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

let%expect_test "winning_move_omok" =
  let positions =
    winning_moves
      ~game_kind:non_win_omok.game_kind
      ~pieces:non_win_omok.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| ()
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win_omok.game_kind
      ~pieces:non_win_omok.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 12) (column 6))) |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    blocking_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    blocking_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {|
  (((row 1) (column 1))) |}]
;;
