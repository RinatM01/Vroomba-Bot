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

open ArrayUtil
open Polygons
open ReadingFiles

(*********************************************)
(*              Helper Functions             *)
(*********************************************)
exception Invalid

let print_float_pair (a, b) = 
  Printf.printf "(%f, %f)\n" a b

let print_point pt = 
  Printf.printf "(%d, %d); " (int_of_float (get_x pt)) (int_of_float (get_y pt))

let print_polygon (p: polygon) =
  List.iter print_point p;
  Printf.printf "\n" 

let print_matrix m =
  for i = Array.length m - 1 downto 0 do
    print_int_array m.(i);
    Printf.printf "\n"
  done

let check_1_negative (p: polygon) : bool =
  let rec walk p = 
    match p with
    | [] -> true
    | pt :: p' ->
      if get_x pt >= 0. && get_y pt >= 0.
      then walk p'
      else false
  in walk p  

let check_2_length (p: polygon) : bool = 
  List.length p >= 4

let check_3_hd_tl (p: polygon) : bool = 
  let rec walk p = 
    match p with
    | [pt] -> pt
    | pt :: p' -> walk p'
    | _ -> error "Not getting a tail for the Polygon in check 3"
  in let tail = walk p in
  let head = List.hd p in
  get_x head = get_x tail || get_y head = get_y tail 

let check_4_walls (p: polygon) : bool = 
  let p' = List.append p [List.hd p] in
  let rec walk p prev_pt coordinates =
    match p with
    | [] -> true
    | pt :: p' ->
      let x = get_x pt
      and x' = get_x prev_pt
      and y = get_y pt
      and y' = get_y prev_pt
      in match x = x', y = y' with
      | true, true -> walk p' pt coordinates
      | false, true -> 
        let rec walk' i coordinates =
          if i = max x x'
          then coordinates
          else let current = (i, y) in
            if List.mem current coordinates
            then raise Invalid
            else walk' (Float.add i 1.) (current :: coordinates)
        in if x < x'
        then let x = Float.add x 1.
          and x' = Float.add x 1. in
          let coordinates' = walk' (min x x') coordinates in
          walk p' pt coordinates'
        else let coordinates' = walk' (min x x') coordinates in
          walk p' pt coordinates'
      | true, false ->
        let rec walk' i coordinates =
          if i = max y y'
          then coordinates
          else let current = (x, i) in
            if List.mem current coordinates
            then raise Invalid
            else walk' (Float.add i 1.) (current :: coordinates)
        in if y < y'
        then let y = Float.add y 1.
          and y' = Float.add y 1. in
          let coordinates' = walk' (min y y') coordinates in
          walk p' pt coordinates'
        else let coordinates' = walk' (min y y') coordinates in
          walk p' pt coordinates'
      | _ -> let () = print_float_pair (x, y) in
        raise Invalid
  in try walk p' (List.hd p') [] with
    Invalid -> false


let negative_to_positive_polygon p =
  let min_i = ref max_float
  and min_j = ref max_float in
  let rec walk p =
    match p with
    | [] -> ()
    | Point (i, j) :: p' ->
      if i < !min_i
      then min_i := i;
      if j < !min_j
      then min_j := j;
      walk p' in
  let rec walk_i p =
    match p with
    | [] -> []
    | Point (i, j) :: p' -> Point (Float.sub i !min_i, j) :: (walk_i p') in
  let rec walk_j p =
    match p with
    | [] -> []
    | Point (i, j) :: p' -> Point (i, Float.sub j !min_j) :: (walk_j p') in
  walk p;
  let new_p = match ((!min_i < 0.), (!min_j < 0.)) with
    | (true, true) -> walk_i (walk_j p)
    | (true, false) -> walk_i p
    | (false, true) -> walk_j p
    | (false, false) -> p in
  new_p

let check_polygon (p: polygon) : bool =
  check_1_negative p && check_2_length p && check_3_hd_tl p && check_4_walls p

let point_to_string pt =
  let x = int_of_float (get_x pt) in
  let y = int_of_float (get_y pt) in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(*********************************************)
(*         Representation of Rooms           *)
(*********************************************)

(* TODO: provide your implementation of internal room data type *)
(* It should describe the room conveniently for solving. *)
type room = int array array

(*  Read a polygon from a string of coordinates as in resources/basic.txt  *)
(*  A string can be ill-formed! *)

let my_string_to_polygon (s : string) : polygon = 
  let ls = my_splitter s in
  let ls' = List.map trimmer ls in
  let ls'' = List.map string_to_pair ls' in
  let points = polygon_of_int_pairs ls'' in
  points

let string_to_polygon (s : string) : polygon option = 
  let ls = my_splitter s in
  let ls' = List.map trimmer ls in
  let ls'' = List.map string_to_pair ls' in
  let points = polygon_of_int_pairs ls'' in
  if not (check_1_negative points)
  then (let points' = negative_to_positive_polygon points in
        if check_polygon points'
        then Some points'
        else None)
  else (if check_polygon points
        then Some points
        else None)

(*  Read all polygons from a file  *)
let file_to_polygons (path: string) : polygon list =
  let ls = read_file_to_strings path in
  let rec walk ls = 
    match ls with
    | [] -> []
    | p :: ls' ->
      let converted_p = string_to_polygon p in
      match converted_p with
      | None -> walk ls'
      | Some p' -> p' :: (walk ls')
  in walk ls

let polygon_to_string (p: polygon) : string =
  let ls = List.map point_to_string p in
  let rec walk ls str = 
    match ls with
    | [] -> str
    | pt :: ls' -> walk ls' (str ^ "; " ^ pt)
  in let raw = walk ls "" in
  String.sub raw 2 (String.length raw - 2)

let write_polygons_to_file (ps: polygon list) (path: string) : unit =
  let lines = List.map polygon_to_string ps in 
  write_strings_to_file path lines

(*********************************************)
(*           Rooms and polygons              *)
(*********************************************)

(*  Convert a polygon to a room data type  *)
let polygon_to_room (p: polygon) : room = 
  (* Vroomba <- 9, uncleaned tile <- 0, cleaned tile <- 1, wall <- 8 *)
  let rec walk p max_x max_y =
    match p with
    | pt :: p' ->
      walk p' (max max_x (get_x pt)) (max max_y (get_y pt))
    | [] -> (int_of_float max_x, int_of_float max_y)
  in let (max_y, max_x) = walk p 0. 0. in
  let m = Array.make_matrix max_x max_y 0 in
  let () = for i = 0 to max_x - 1 do
      for j = 0 to max_y - 1 do
        if (point_within_polygon p (Point (float_of_int j, float_of_int i)) 
            && point_within_polygon p (Point (float_of_int (j + 1), float_of_int i))
            && point_within_polygon p (Point (float_of_int j, float_of_int (i + 1)))
            && point_within_polygon p (Point (float_of_int (j + 1), float_of_int (i + 1)))) = false
        then m.(i).(j) <- 8
      done 
    done in
  let counter = ref 0 in
  let () = while !counter < Array.length m.(0) do
      if m.(0).(!counter) = 0
      then (let () = m.(0).(!counter) <- 9 in counter := !counter + Array.length m.(0))
      else counter := !counter + 1
    done in
  m

(*  Convert a room to a list of polygon coordinates  *)

let room_to_polygon_aux (r: room) : polygon = 
  let ht = Hashtbl.create 50 in 
  let check i j = 
    if Hashtbl.mem ht (i, j) 
    then Hashtbl.replace ht (i, j) (Hashtbl.find ht (i, j) + 1) 
    else Hashtbl.add ht (i, j) 1 in 
  let () = for i = 0 to Array.length r.(0) - 1 do 
      for j = 0 to Array.length r - 1 do 
        if r.(j).(i) != 8 
        then  
          (check i j; 
           check (i + 1) j; 
           check i (j + 1); 
           check (i + 1) (j + 1);) 
      done 
    done in  
  let p = ref [] in 
  let () = Hashtbl.iter (fun (i, j) v -> if v mod 2 = 1 then p := Point (float_of_int i, float_of_int j) :: !p) ht in 
  !p

let return_closer comparator closest_so_far curr is_same_x= 
  match comparator, closest_so_far, curr with
  | Point (comparator_x, comparator_y), Point (closest_so_far_x, closest_so_far_y), Point (curr_x,curr_y)
    -> if is_same_x
    then if Float.abs(Float.sub comparator_y closest_so_far_y) < Float.abs(Float.sub comparator_y curr_y)
      then closest_so_far
      else curr
    else if Float.abs(Float.sub comparator_x closest_so_far_x) < Float.abs(Float.sub comparator_x curr_x)
    then closest_so_far
    else curr

let closest_same_x poly_list curr = 
  match curr with
  |Point (x,y) -> let rec walk poly_list a = 
                    match poly_list with
                    | Point (poly_x, poly_y) :: poly_list' 
                      -> if poly_x = x
                      then walk poly_list' (return_closer curr a (Point (poly_x, poly_y)) true)
                      else walk poly_list' a
                    | [] -> a
    in walk poly_list (Point (1000.0,1000.0))

let closest_same_y poly_list curr = 
  match curr with
  |Point (x,y) -> let rec walk poly_list a = 
                    match poly_list with
                    |Point (poly_x, poly_y) :: poly_list' 
                      -> if poly_y = y
                      then walk poly_list' (return_closer curr a (Point (poly_x, poly_y)) false)
                      else walk poly_list' a
                    | [] -> a
    in walk poly_list (Point (1000.0,1000.0))

let delete_point poly point = 
  match point with
  | Point (x,y) -> let rec walk poly = 
                     match poly with
                     | Point (poly_point_x, poly_point_y) :: poly' -> if (Point (poly_point_x, poly_point_y)) = Point (x,y)
                       then walk poly'
                       else Point (poly_point_x, poly_point_y) :: (walk poly')
                     | [] -> []
    in walk poly 


let sort_polygon poly = 
  let rec walk remain_points sorted curr counter =
    match remain_points with
    | [] -> sorted
    | _ -> if (counter mod 2 = 0)
      then let closest = closest_same_x remain_points curr in 
        walk (delete_point remain_points closest) (List.append sorted (closest :: [])) closest (counter+1)
      else let closest = closest_same_y remain_points curr in 
        walk (delete_point remain_points closest) (List.append sorted (closest :: [])) closest (counter+1)
  in walk poly [] (Point (0.0, 0.0)) 0

let room_to_polygon (r: room) : polygon = 
  let unsorted = room_to_polygon_aux r in
  sort_polygon unsorted

(* let%test "polygon" = 
   let a = file_to_polygons "/home/zhzchen327/ds_algo_dir/final-project-2022-lilzichen_rinatlamar_renyudogg-1/resources/rooms.txt" in
   List.iter (fun p -> print_matrix (polygon_to_room p); print_endline "") a;
   true *)


(* let%test "polygon2" = 
   let p = my_string_to_polygon "(2, 0); (3, 0); (3, 3); (0, 3); (0, 6); (2, 6)" in
   check_polygon p *)


(* let%test "polygon_to_room" = 
   let p = my_string_to_polygon "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   let () = print_matrix (polygon_to_room p) in
   true *)


(* let%test "delete a point" =
   let p = my_string_to_polygon "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   print_polygon p;
   let p_deleted = delete_point p (Point (0.0,0.0)) in
   print_polygon p_deleted;
   true *)

(* let%test "room_to_polygon" = 
   let p = my_string_to_polygon "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
   print_polygon p;
   let r = polygon_to_room p in
   print_matrix r;
   let () = print_polygon (room_to_polygon r) in
   true *)

