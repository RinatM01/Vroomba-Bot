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
open ArrayUtil
open Rooms

(*********************************************)
(*       Automated generation of rooms       *)
(*********************************************)

(*  size is the maximal span of the room along both X and Y dimensions *)
(*  Example generate_random_room 4 should return a room that fits into a
    4x4 square. *)
let print_matrix m =
  for i = Array.length m - 1 downto 0 do
    print_int_array m.(i);
    Printf.printf "\n"
  done

(*chooses a random item out of given option*)
let rand4 p1 p2 p3 p4 =
  Random.self_init (); 
  match Random.int 4 with
  | 0 -> p1
  | 1 -> p2
  | 2 -> p3
  | 3 -> p4
  | _ -> raise Invalid 

let rand3 p1 p2 p3 = 
  Random.self_init (); 
  match Random.int 3 with
  | 0 -> p1
  | 1 -> p2
  | 2 -> p3
  | _ -> raise Invalid 

let rand2 p1 p2 = 
  Random.self_init (); 
  match Random.int 2 with
  | 0 -> p1
  | 1 -> p2
  | _ -> raise Invalid 

let point_within_matrix_and_wall m x y = 
  let max_x = (Array.length m) - 1 in
  let max_y = (Array.length m.(0)) - 1 in
  match x < 0, y <0, x > max_x, y > max_y with

  | false, false, false, false -> if m.(x).(y) = 8
    then true
    else false
  | _ -> false

let point_within_matrix_and_empty m x y = 
  let max_x = (Array.length m) - 1 in
  let max_y = (Array.length m.(0)) - 1 in
  match x < 0, y <0, x > max_x, y > max_y with
  | false, false, false, false -> if m.(x).(y) = 0
    then true
    else false
  | _ -> false
(* makes random step in random direction if possible *)
let make_random_step m x y =
  if point_within_matrix_and_wall m (x-1) y
  then if point_within_matrix_and_wall m (x+1) y 
    then if point_within_matrix_and_wall m x (y-1)
      then if point_within_matrix_and_wall m x (y+1)
        then Some (rand4 (x-1,y) (x+1,y) (x,y-1) (x,y+1))
        else Some (rand3 (x-1,y) (x+1,y) (x,y-1))
      else if point_within_matrix_and_wall m x (y+1)
      then Some (rand3 (x-1,y) (x+1,y) (x,y+1))
      else Some (rand2 (x-1,y) (x+1,y))
    else if point_within_matrix_and_wall m x (y-1)
    then if point_within_matrix_and_wall m x (y+1)
      then Some (rand3 (x-1,y) (x,y-1) (x,y+1))
      else Some (rand2 (x-1,y) (x,y-1))
    else if point_within_matrix_and_wall m x (y+1)
    then Some (rand2 (x-1,y) (x,y+1))
    else Some (x-1,y)
  else if point_within_matrix_and_wall m (x+1) y 
  then if point_within_matrix_and_wall m x (y-1)
    then if point_within_matrix_and_wall m x (y+1)
      then Some (rand3 (x+1,y) (x,y-1) (x,y+1))
      else Some (rand2 (x+1,y) (x,y-1))
    else if point_within_matrix_and_wall m x (y+1)
    then Some (rand2 (x+1,y) (x,y+1))
    else Some (x+1,y)
  else if point_within_matrix_and_wall m x (y-1)
  then if point_within_matrix_and_wall m x (y+1)
    then Some (rand2 (x,y-1) (x,y+1))
    else Some (x,y-1)
  else if point_within_matrix_and_wall m x (y+1)
  then Some (x,y+1)
  else None

let remove_middle_walls m = 
  let max_x = (Array.length m) - 1 in
  let max_y = (Array.length m.(0)) - 1 in
  for x=0 to max_x do
    for y=0 to max_y do
      if (m.(x).(y) = 8) && (point_within_matrix_and_empty m (x-1) y) 
         && (point_within_matrix_and_empty m (x+1) y) 
         && (point_within_matrix_and_empty m x (y-1)) 
         && (point_within_matrix_and_empty m x (y+1))
         && (point_within_matrix_and_empty m (x-1) (y-1))
         && (point_within_matrix_and_empty m (x+1) (y-1))
         && (point_within_matrix_and_empty m (x-1) (y+1))
         && (point_within_matrix_and_empty m (x+1) (y+1))
      then m.(x).(y) <- 0
    done
  done;
  m

let generate_random_room (size : int) : room = 
  let s = Stack.create () in
  Stack.push (0,0) s;
  Random.self_init ();
  let steps = Random.int (size*size) in
  let m = Array.make_matrix size size 8 in
  let rec walk steps_left curr_x curr_y =
    m.(curr_x).(curr_y) <- 0;
    (* Printing the process of room generation *)
    (* print_matrix m; *)
    (* Printf.printf "\n"; *)
    if steps_left > 0
    then match make_random_step m curr_x curr_y with
      | Some (x,y) -> Stack.push (curr_x, curr_y) s; walk (steps_left - 1) x y
      | None -> match Stack.pop s with
        | (x,y) -> walk steps_left x y
    else m
  in let r = remove_middle_walls (walk steps 0 0) in
  let counter = ref 0 in
  let () = while !counter < Array.length r.(0) do
      if r.(0).(!counter) = 0
      then (let () = r.(0).(!counter) <- 9 in counter := !counter + Array.length r.(0))
      else counter := !counter + 1
    done in
  r

(* Define what it means to the room to be valid (e.g., no lacunas,
   obstacles, there is a place for initial Vroomba position, etc). *)
let valid_room (r: room) : bool = 
  check_polygon (room_to_polygon r)

(*********************************************)
(*                     Tests                 *)
(*********************************************)

(* let%test "rando works" =
   let p1 = (0,0)
   and p2 = (1,1)
   and p3 = (2,2)
   and p4 = (3,3) in
   match (rand4 p1 p2 p3 p4) with
    | (x,y) -> Printf.printf "%d" x; true 
    | _ -> false *)

(* let%test "random step works" = 
   let m = Array.make_matrix 4 4 0 in
   m.(1).(1) <- 1;
   print_matrix m;
   match make_random_step m 1 1 with
   | Some (x, y) -> m.(x).(y) <- 1; print_matrix m; true
   | None -> true *)

(* let%test "Remove middle walls" = 
   let m = Array.make_matrix 4 4 0 in
   m.(1).(1) <- 8;
   m.(3).(0) <- 8;
   print_matrix m;
   Printf.printf "\n";
   let m_new = remove_middle_walls m in
   print_matrix m_new;
   true *)

(* let%test "Generated room is valid" = 
   let r = generate_random_room 5 in
   valid_room r *)


(* TODO: add more tests *)