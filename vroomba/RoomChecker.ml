
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

open Util
open Rooms
open ReadingFiles

(*********************************************)
(*         Movements of Vroomba              *)
(*********************************************)

type move = 
  | Up 
  | Left 
  | Down 
  | Right

(* Print the move *)
let pp_move = function
  | Up -> "W"
  | Left -> "A"
  | Down -> "S"
  | Right -> "D"

let print_solution sol =
  List.iter (fun move -> print_endline (pp_move move)) sol

let start r = 
  let coord = ref (0, 0) in
  for i = 0 to Array.length r.(0) - 1 do
    for j = 0 to Array.length r - 1 do
      if r.(j).(i) = 9
      then coord := (i, j)
    done
  done;
  !coord

(*  TODO: Implement me!  *)
(* This is a data type, representing the state of the room at a
   certain point of cleaning. It should include the room and keep
   track of Vroomba's current position, parts that are already cleaned
   and that are remaining to be cleaned. Use this data type internally
   in the function `check_solution` *)

type state = room (* You should change this definition *)

(*********************************************)
(*            Checking solution              *)
(*********************************************)

(*  Get a trace of Vroomba from a string  *)
(*  A string can be ill-formed! *)
exception Ill

let string_to_solution (s: string) : move list option = 
  if s = ""
  then None
  else 
    let walk s =
      let moves = ref [] in 
      for i = 0 to String.length s - 1 do
        let move = match s.[i] with
          | 'W' -> Up
          | 'A' -> Left
          | 'S' -> Down
          | 'D' -> Right 
          | _ -> raise Ill in
        moves := List.append !moves [move] 
      done;
      Some !moves in
    try walk s with
    | Ill -> None

let file_to_solutions (path: string) : move list list =
  let ls = read_file_to_strings path in
  let rec walk ls = 
    match ls with
    | [] -> []
    | s :: ls' ->
      let converted_s = string_to_solution s in
      match converted_s with
      | None -> walk ls'
      | Some s' -> s' :: (walk ls')
  in walk ls

(*  Check that the sequence of moves is valid  *)
exception Uncleaned

let check_complete r = 
  let check r =
    let ok = true in
    for i = 0 to Array.length r - 1 do
      for j = 0 to Array.length r.(0) - 1 do
        if r.(i).(j) = 0
        then raise Uncleaned
      done
    done;
    ok in
  try check r with
  | Uncleaned ->
    false

exception OutOfRoom

let print_bool b = 
  match b with
  | true -> print_string "true"
  | false -> print_string "false" 

let update_clean ((i, j), state) =
  let if_can_clean = Array.make 8 true 
  and stored_values = ref [||] 
  and counter = ref 0 in
  (for m = j - 1 to j + 1 do
     for n = i - 1 to i + 1 do
       if m >= 0 && n >= 0 && m < Array.length state && n < Array.length state.(0)
       then (let stored_value = state.(m).(n) in
             if not (m = j && n = i)
             then stored_values := Array.append !stored_values [| stored_value |])
       else stored_values := Array.append !stored_values [| -1 |]
     done
   done);
  (if !stored_values.(1) = 8
   then (if_can_clean.(0) <- false; if_can_clean.(2) <- false));
  (if !stored_values.(3) = 8
   then (if_can_clean.(0) <- false; if_can_clean.(5) <- false));
  (if !stored_values.(4) = 8
   then (if_can_clean.(2) <- false; if_can_clean.(7) <- false));
  (if !stored_values.(6) = 8
   then (if_can_clean.(5) <- false; if_can_clean.(7) <- false));
  (for m = j - 1 to j + 1 do
     for n = i - 1 to i + 1 do
       if not (m = j && n = i)
       then (if if_can_clean.(!counter) && !stored_values.(!counter) = 0
             then state.(m).(n) <- 1;
             counter := !counter + 1)
     done
   done);
  ((i, j), state)  

let update (r: (int * int) * state) (m: move) : (int * int) * state =
  let valid_step ((i, j), state) =
    i >= 0 && j >= 0 && i < Array.length state.(0) && j < Array.length state in 
  let update_v ((i, j), state) =
    let new_coord = (match m with
        | Up -> 
          if valid_step ((i, j + 1), state) 
          then (state.(j + 1).(i) <- 9; state.(j).(i) <- 1; (i, j + 1)) 
          else raise OutOfRoom
        | Down -> 
          if valid_step ((i, j - 1), state) 
          then (state.(j - 1).(i) <- 9; state.(j).(i) <- 1; (i, j - 1)) 
          else raise OutOfRoom
        | Left -> 
          if valid_step ((i - 1, j), state) 
          then (state.(j).(i - 1) <- 9; state.(j).(i) <- 1; (i - 1, j)) 
          else raise OutOfRoom
        | Right -> 
          if valid_step ((i + 1, j), state) 
          then (state.(j).(i + 1) <- 9; state.(j).(i) <- 1; (i + 1, j)) 
          else raise OutOfRoom) in
    (new_coord, state) in
  update_clean (update_v r)

(* let update_clean_v2 i j r =
   let if_can_clean = Array.make 8 true 
   and stored_values = ref [||] 
   and counter = ref 0 in
   (for m = j - 1 to j + 1 do
     for n = i - 1 to i + 1 do
       if m >= 0 && n >= 0 && m < Array.length state && n < Array.length state.(0)
       then (let stored_value = state.(m).(n) in
             if not (m = j && n = i)
             then stored_values := Array.append !stored_values [| stored_value |])
       else stored_values := Array.append !stored_values [| -1 |]
     done
   done);
   (if !stored_values.(1) = 8
   then (if_can_clean.(0) <- false; if_can_clean.(2) <- false));
   (if !stored_values.(3) = 8
   then (if_can_clean.(0) <- false; if_can_clean.(5) <- false));
   (if !stored_values.(4) = 8
   then (if_can_clean.(2) <- false; if_can_clean.(7) <- false));
   (if !stored_values.(6) = 8
   then (if_can_clean.(5) <- false; if_can_clean.(7) <- false));
   (for m = j - 1 to j + 1 do
     for n = i - 1 to i + 1 do
       if not (m = j && n = i)
       then (if if_can_clean.(!counter) && !stored_values.(!counter) = 0
             then state.(m).(n) <- 1;
             counter := !counter + 1)
     done
   done);
   ((i, j), state)  

   let update_v2 i j r =
   let valid_step ((i, j), state) =
    i >= 0 && j >= 0 && i < Array.length state.(0) && j < Array.length state in 
   let update_v ((i, j), state) =
    let new_coord = (match m with
        | Up -> 
          if valid_step ((i, j + 1), state) 
          then (state.(j + 1).(i) <- 9; state.(j).(i) <- 1; (i, j + 1)) 
          else raise OutOfRoom
        | Down -> 
          if valid_step ((i, j - 1), state) 
          then (state.(j - 1).(i) <- 9; state.(j).(i) <- 1; (i, j - 1)) 
          else raise OutOfRoom
        | Left -> 
          if valid_step ((i - 1, j), state) 
          then (state.(j).(i - 1) <- 9; state.(j).(i) <- 1; (i - 1, j)) 
          else raise OutOfRoom
        | Right -> 
          if valid_step ((i + 1, j), state) 
          then (state.(j).(i + 1) <- 9; state.(j).(i) <- 1; (i + 1, j)) 
          else raise OutOfRoom) in
    (new_coord, state) in
   update_clean (update_v r) *)

let check_solution (r: room) (moves: move list) : bool = 
  let v_start_pos = start r in
  let (_, r') = update_clean (v_start_pos, r) in
  let rec walk moves v_pos_and_state =
    match moves with
    | [] -> v_pos_and_state
    | move :: moves' ->
      let updated = update v_pos_and_state move in
      walk moves' updated in
  let (_, final_state) = 
    try walk moves (v_start_pos, r') with 
    | _ -> ((0, 0), [|[|0|]|]) in
  (* print_matrix final_state; *)
  check_complete final_state

(*  Top-level validator  *)
let validate r s = 
  match string_to_solution s with
  | None -> false
  | Some moves -> check_solution r moves


(* let%test _ = 
   let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   let room = string_to_polygon s |> get_exn |> polygon_to_room in
   validate room "WDDDDDD" 

   (* TODO: Add more tests *)                                                 

   let%test "string_to_solution_2" =
   string_to_solution "WASDF" = None

   let%test _ = 
   let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   let room = string_to_polygon s |> get_exn |> polygon_to_room in
   validate room "WDDDDDDD" 

   let%test _ = 
   let s = "(0, 0); (1, 0); (1, 3); (0, 3)" in
   let room = string_to_polygon s |> get_exn |> polygon_to_room in
   validate room "WW"  *)

(* let%test "big_room" =
  let room = string_to_polygon (read_file_to_single_string "/home/zhzchen327/ds_algo_dir/final-project-2022-lilzichen_rinatlamar_renyudogg-1/resources/storage2.txt") in
  let solution = read_file_to_single_string "/home/zhzchen327/ds_algo_dir/final-project-2022-lilzichen_rinatlamar_renyudogg-1/resources/storage.txt" in
  let room' = match room with
    | Some x -> polygon_to_room x
    |None -> error "what" in
  validate room' solution *)