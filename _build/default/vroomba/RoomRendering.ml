(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2020 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

include Util
open Rooms
open RoomChecker
open RoomSolver
open RoomGenerator
open GraphicUtil
open ReadingFiles
open Graphics
include Polygons
include Points


(*********************************************)
(*           Gamifying the solver            *)
(*********************************************)

(* TODO: Implement more functions! *)

let rec concat moves =
  match moves with
  | [] -> ""
  | move :: moves' -> pp_move move ^ (concat moves')

let convert_mll_to_strings (moves: move list list) =
  List.map (fun x -> concat x) moves

let write_solution_to_file (moves : move list list) (path : string) : unit =
  write_strings_to_file path (convert_mll_to_strings moves)

(* TODO: feel free to modify this function to add more parameters
   necessary for tracking your game state *)
(*let rec wait_until_q_pressed _ =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' 
  then exit 0
  else begin
      (* TODO: Implement the movement logic *)
      wait_until_q_pressed ()
    end*)

(*  Top-level file for starting the game  *)
let break_big_polygon_up polygon =
  let polygon_list = ref [] in
  let room = polygon_to_room polygon in
  (*print_matrix room;*)
  let () =
    for i = 0 to Array.length room.(0) - 1 do
      for j = 0 to Array.length room - 1 do
        let float_i = float_of_int i
        and float_j = float_of_int j in
        let small_p = [Point (float_i, float_j); Point (Float.add float_i 1., float_j); Point (Float.add float_i 1., Float.add float_j 1.); Point (float_i, Float.add float_j 1.)] in
        polygon_list := List.append !polygon_list [small_p] 
      done
    done in
    !polygon_list

(*let%test "break" =
 let polygon_list = break_big_polygon_up [Point(0.,0.); Point(2.,0.); Point(2.,2.); Point(0.,2.)] in
 List.iter print_polygon polygon_list; true*)

(*let render_games (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Example room, resized and shifted to look nice *)
  let p = [(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)] 
          |> polygon_of_int_pairs 
          |> resize_polygon 80.
          |> shift_polygon (-300., -100.)
  in
  draw_polygon p;
  let problems = file_to_polygons input_path
  in (match problems with
      |[] -> raise "No rooms to load"
      |current_room_p::next_room_p::rest_of_rooms_p -> (let current_room = polygon_to_room current_room_p
                                                        and let next_room = polygon_to_room next_room_p
                                                                in open_graph " 800x600";
                                                                   draw_polygon (break_big_polygon_up current_room_p);
                                                                   wait_until_q_pressed current_room)
  wait_until_q_pressed () *)

(* let%test "draw_polygon" =
  open_graph " 500x500";
  List.iter draw_polygon (break_big_polygon_up [Point(0.,0.); Point(100.,0.); Point(100.,100.); Point(100.,100.)]);
  wait_until_q_pressed (); true  *)

(*let%test "draw_stuff" = 
open_graph " 800x600";
  let p = [(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)] 
          |> polygon_of_int_pairs 
          |> resize_polygon 80.
          |> shift_polygon (-300., -100.)
  in
  draw_polygon p;
  wait_until_q_pressed (); true *)

let int_of_polygon_pairs p =
  List.map (function Point (x, y) ->
      (int_of_float x, int_of_float y)) p

let get_x_list_from_list_of_int_pairs list_of_int_pairs =
  let rec traverse list_of_int_pairs a =
    match list_of_int_pairs with
    |[] -> a
    |x::xs -> traverse xs (fst x::a)
  in traverse list_of_int_pairs []

let get_y_list_from_list_of_int_pairs list_of_int_pairs =
  let rec traverse list_of_int_pairs a =
    match list_of_int_pairs with
    |[] -> a
    |x::xs -> traverse xs (snd x::a)
  in traverse list_of_int_pairs []

let get_max_from_list list =
  let rec traverse list a =
    match list with
    |[] -> a
    |x::xs -> traverse xs (max x a)
  in traverse list (-1000)

let get_min_from_list list =
  let rec traverse list a =
    match list with
    |[] -> a
    |x::xs -> traverse xs (min x a)
  in traverse list 1000

let resize_how_much polygon =
  Float.of_int (600/
                  (max (get_max_from_list (get_x_list_from_list_of_int_pairs (int_of_polygon_pairs polygon)))
                     (get_max_from_list (get_y_list_from_list_of_int_pairs (int_of_polygon_pairs polygon)))))

let shift_how_much polygon =
  let resized = resize_how_much polygon
  and centre_of_polygon = (0.5 *.
                             float_of_int (get_max_from_list (get_x_list_from_list_of_int_pairs (int_of_polygon_pairs polygon))) +.
                             float_of_int (get_min_from_list (get_x_list_from_list_of_int_pairs (int_of_polygon_pairs polygon))),
                           0.5 *.
                             float_of_int (get_max_from_list (get_y_list_from_list_of_int_pairs (int_of_polygon_pairs polygon))) +.
                             float_of_int (get_min_from_list (get_y_list_from_list_of_int_pairs (int_of_polygon_pairs polygon))))
  in (-. resized *. (fst centre_of_polygon) , -. resized *. (snd centre_of_polygon))

let draw_room polygon =
  clear_graph();
  let p = polygon
          |> resize_polygon (resize_how_much polygon)
          |> shift_polygon (shift_how_much polygon)
  in draw_polygon p

exception No_polygon

let get_first_point_from_polygon_and_turn_it_into_a_polygon polygon =
  match polygon with
  |[] -> raise No_polygon
  |Point (x,y) :: xs -> [Point (x,y); Point(x +. 1., y); Point(x +. 1., y +. 1.); Point(x, y +. 1.)]

let resize_and_shifted big_polygon want_polygon =
  let resize_factor = resize_how_much big_polygon
  and shift_factor = shift_how_much big_polygon
  in List.map (function Point (x,y) -> Point ( x *. resize_factor +. (fst shift_factor), y *. resize_factor +. (snd shift_factor) )) want_polygon

let set_vroomba_position polygon =
  let vroom_polygon = get_first_point_from_polygon_and_turn_it_into_a_polygon polygon
  in (let vroomba_polygon = resize_and_shifted polygon vroom_polygon
      in set_color Graphics.red;
         fill_poly (Array.of_list (int_of_polygon_pairs vroomba_polygon)))

(*let draw_room_squares polygon =
  clear_graph();
  let small_p = break_big_polygon_up polygon
  in (let rec traverse small_p =
        match small_p with
        |[] -> draw_polygon polygon
        |x::xs -> let p = x
                          |> resize_polygon (resize_how_much polygon)
                          |> shift_polygon (shift_how_much polygon)
                  in draw_polygon p;
                  traverse xs
      in traverse small_p) *)

let rec wait_until_q_pressed _ =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q'
  then exit 0

(*let%test "draw room" =
  open_graph " 800x600";
  List.iter draw_room (break_big_polygon_up [Point(0.,0.); Point(10.,0.); Point(10.,10.); Point(10.,10.)]);
  wait_until_q_pressed (); true *)

(* let%test "draw room" =
  open_graph " 800x600";
  draw_room [Point(0.,0.); Point(6.,0.); Point(6.,1.); Point(8.,1.); Point(8.,2.); Point(6.,2.); Point(6.,3.); Point(0.,3.)];
  set_vroomba_position [Point(0.,0.); Point(6.,0.); Point(6.,1.); Point(8.,1.); Point(8.,2.); Point(6.,2.); Point(6.,3.); Point(0.,3.)];
  wait_until_q_pressed (); true *)

(*let%test "draw room" =
  open_graph " 800x600";
  draw_polygon [Point (-300., -112.5); Point (-225., -112.5); Point (-225., -37.5); Point (-300., -37.5)]
  wait_until_q_pressed (); true*)

(*let%test "draw room" =
  open_graph " 800x600";
  draw_polygon [Point (-300., -112.5); Point (-225., -112.5); Point (-225., -37.5); Point (-300., -37.5)];
  wait_until_q_pressed (); true*)




(*
(*********** Zichen's version ***********)

let get_maxes_and_mins polygon = 
  let max_x = ref min_float
  and min_x = ref max_float
  and max_y = ref min_float
  and min_y = ref max_float in
  let rec walk p =
    match p with
    | [] -> ()
    | Point (x, y) :: p' ->
      if x > !max_x then max_x := x;
      if x < !min_x then min_x := x;
      if y > !max_y then max_y := y;
      if y < !min_y then min_y := y;
      walk p' in
  walk polygon;
  (* return a four-element tuple with max & min x and max & min y *)
  (!max_x, !min_x, !max_y, !min_y)

let resize_and_shift polygon =
  let (max_x, min_x, max_y, min_y) = get_maxes_and_mins polygon in
  let resize_factor = Float.div 600. (max max_x max_y)
  and shift_offset = 0. (* didn't understand your logic so pls fill in *) in
  (* return the resized and shifted polygon *)
  List.map 
    (* shift first then resize *)
    (fun (Point (x, y)) -> Point (Float.mul resize_factor (Float.add shift_offset x), Float.mul resize_factor (Float.add shift_offset y))) 
    polygon

let draw_room polygon (* this is the big polygon *) =
  let small_polygon_list = break_big_polygon_up polygon in
  (* resize and shift *)
  let resized_and_shifted_small_polygons = List.map resize_and_shift small_polygon_list in
  (* iterate through the list and draw each small polygons *)
  List.iter draw_polygon resized_and_shifted_small_polygons
*)
