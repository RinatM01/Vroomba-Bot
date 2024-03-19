(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2021 Ilya Sergey

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

open Util
open Rooms
open RoomChecker
open RoomGenerator
open Stacks

(*********************************************)
(*              Room solver                  *)
(*********************************************)

(* This is a complex task. Feel free to introduce whatever functions
   and data types you consider necessary, and also rely on data
   structures and algorithms from the lectures (see folder `lib` of
   this project). *)

(* Solve the room and produce the list of moves. *)
(* Make use of RoomChecker.state state type internally in your solver *)

let print_moves moves =
  List.iter (fun x -> print_string ((pp_move x) ^ " ")) moves

let is_safe i j r =
  i >= 0 && j >= 0 && i < Array.length r.(0) && j < Array.length r 

exception Found

let move_to (start_x, start_y) (dest_x, dest_y) r : move list =
  let visited = Array.make_matrix (Array.length r) (Array.length r.(0)) false in 
  let move_list = ref [] in
  let rec remove_list_tail l =
    match l with
    | [_] -> []
    | e :: l' -> e :: (remove_list_tail l')
    | [] -> [] in
  let rec find_path move (current_x, current_y) visited =
    if is_safe current_x current_y r && r.(current_y).(current_x) != 8 && (not visited.(current_y).(current_x))
    then (visited.(current_y).(current_x) <- true;
          move_list := List.append !move_list [move];
          (if current_x = dest_x && current_y = dest_y
           then true
           else let right = find_path Right (current_x + 1, current_y) visited in
             if right then true 
             else let up = find_path Up (current_x, current_y + 1) visited in
               if up then true 
               else let left = find_path Left (current_x - 1, current_y) visited in
                 if left then true else
                   let down = find_path Down (current_x, current_y - 1) visited in
                   if down then true else let () = move_list := remove_list_tail !move_list in false))
    else false in
  let if_have_path = find_path Right (start_x, start_y) visited in
  if if_have_path 
  then List.tl !move_list
  else error "no path between the two points, wrong room!"

let check_surrounding i j r s =
  (if is_safe (i + 1) j r
   then if r.(j).(i + 1) = 0
     then ArrayBasedStack.push s (i + 1, j));
  (if is_safe i (j + 1) r
   then if r.(j + 1).(i) = 0
     then ArrayBasedStack.push s (i, j + 1)); 
  (if is_safe (i - 1) j r
   then if r.(j).(i - 1) = 0
     then ArrayBasedStack.push s (i - 1, j)); 
  (if is_safe i (j - 1) r
   then if r.(j - 1).(i) = 0
     then ArrayBasedStack.push s (i, j - 1))

let check_more_surrounding i j r s =
  for m = i - 2 to i + 2 do
    for n = j - 2 to j + 2 do
      if is_safe m n r && not (m = i && n = j)
      then if r.(n).(m) = 0
        then ArrayBasedStack.push s (m, n)
    done
  done

let solve_room_v2 (r: room) : move list = 
  let s = ArrayBasedStack.mk_stack 10000
  and moves = ref [] in
  let rec walk i j s r =
    let (_, r') = update_clean ((i, j), r) in
    check_more_surrounding i j r' s;
    if ArrayBasedStack.is_empty s
    then true
    else (let (i', j') = match ArrayBasedStack.pop s with
        | Some x -> x
        | None -> error "the stack is empty but solve_room is not terminated"
       in 
       if r'.(j').(i') != 0
       then walk i j s r'
       else (moves := List.append !moves (move_to (i, j) (i', j') r');
             r'.(j').(i') <- 1;
             walk i' j' s r'))
  in let (start_i, start_j) = start r in
  let finished = walk start_i start_j s r in
  if finished
  then !moves
  else error "solve_room didn't finish"

let solve_room (r: room) : move list = 
  let s = ArrayBasedStack.mk_stack 1000
  and moves = ref [] in
  let rec walk i j s =
    check_surrounding i j r s;
    if ArrayBasedStack.is_empty s
    then true
    else (let (i', j') = match ArrayBasedStack.pop s with
        | Some x -> x
        | None -> error "the stack is empty but solve_room is not terminated"
       in moves := List.append !moves (move_to (i, j) (i', j') r);
       r.(j').(i') <- 1;
       walk i' j' s)
  in let (start_i, start_j) = start r in
  let finished = walk start_i start_j s in
  if finished
  then !moves
  else error "solve_room didn't finish"

let rec concat moves = 
  match moves with
  | [] -> ""
  | move :: moves' -> pp_move move ^ (concat moves')

let convert_all_to_strings moves =
  List.map (fun x -> concat x) moves

(*********************************************)
(*               Testing                     *)
(*********************************************)


let%test "Randomised solver testing" = 
  let r = generate_random_room 30 in
  let moves = solve_room_v2 r in
  check_solution r moves


(* TODO: Add more tests *)
(* let%test "check_moves" = 
   let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   let room = string_to_polygon s |> get_exn |> polygon_to_room in
   print_matrix room;
   let moves = move_to (0, 0) (2, 1) room in
   print_moves moves;
   true *)


(* let%test "convert_all_to_strings" =
   let r = [[Up; Up; Up; Up]; [Right]; [Down]; [Left]] in
   let moves = convert_all_to_strings r in
   List.iter (fun x -> print_endline x) moves;
   true *)