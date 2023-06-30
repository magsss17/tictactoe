open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let avail = Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces in
  if List.length avail = 0
  then failwith "no available positions"
  else List.random_element_exn avail
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if List.is_empty winning_moves
  then random_move_strategy ~game_kind ~pieces
  else List.random_element_exn winning_moves
;;

let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)

let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  let blocking_moves =
    Tic_tac_toe_exercises_lib.blocking_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty winning_moves)
  then List.random_element_exn winning_moves
  else if not (List.is_empty blocking_moves)
  then List.random_element_exn blocking_moves
  else random_move_strategy ~game_kind ~pieces
;;

let _ = pick_winning_move_or_block_if_possible_strategy

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  match Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces with
  | Game_over { winner = Some p } ->
    if Piece.equal me p then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

let rec minimax
  ~(node : Position.t)
  ~(depth : int)
  ~(maximizing_player : bool)
  ~(game_kind : Game_kind.t)
  ~(me : Piece.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(player : Piece.t)
  : float
  =
  let temp = Map.set pieces ~key:node ~data:me in
  if depth = 0
     ||
     match Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces with
     | Game_continues -> false
     | _ -> true
  then score ~game_kind ~me:player ~pieces:temp
  else if maximizing_player
  then (
    let _value = ref Float.neg_infinity in
    let children =
      Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces:temp
    in
    children
    |> List.map ~f:(fun new_pos ->
         minimax
           ~node:new_pos
           ~depth:(depth - 1)
           ~maximizing_player:false
           ~me:(Piece.flip me)
           ~pieces:temp
           ~game_kind
           ~player)
    |> List.fold ~init:Float.neg_infinity ~f:Float.max
    (* List.iter children ~f:(fun x -> value := Core.Float.max !value
       (minimax ~node:x ~depth:(depth - 1) ~maximizing_player:false
       ~game_kind ~me:(Piece.flip me) ~pieces:temp ~player)); !value) *))
  else (
    let _value = ref Float.infinity in
    let children =
      Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces:temp
    in
    children
    |> List.map ~f:(fun new_pos ->
         minimax
           ~node:new_pos
           ~depth:(depth - 1)
           ~maximizing_player:true
           ~me
           ~pieces:temp
           ~game_kind
           ~player)
    |> List.fold ~init:Float.infinity ~f:Float.min)
;;

(* List.iter children ~f:(fun x -> value := Core.Float.min !value (minimax
   ~node:x ~depth:(depth - 1) ~maximizing_player:true ~game_kind ~me
   ~pieces:temp ~player)); !value) *)

let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let game_kind = game_state.game_kind in
  let pieces = game_state.pieces in
  let available =
    Tic_tac_toe_exercises_lib.available_moves
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
  in
  match
    available
    |> List.map ~f:(fun pos ->
         ( minimax
             ~node:pos
             ~depth:11
             ~maximizing_player:false
             ~me
             ~pieces
             ~game_kind
             ~player:me
         , pos ))
    |> List.max_elt ~compare:(fun (v1, _pos1) (v2, _pos2) ->
         Float.compare v1 v2)
  with
  | Some (_v1, pos) -> pos
  | None -> random_move_strategy ~game_kind ~pieces
;;
