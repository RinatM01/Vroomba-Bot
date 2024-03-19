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

open Printf
open Util
open ReadingFiles
open Rooms
open RoomGenerator
open RoomChecker
open RoomSolver
open RoomRendering


let rec concat moves = 
  match moves with
  | [] -> ""
  | move :: moves' -> pp_move move ^ (concat moves')

let convert_all_to_strings moves =
  List.map (fun x -> concat x) moves

let () =
  if Array.length (Sys.argv) < 4 
  then begin
    print_endline "Not enough input provided";
    print_endline "Possilbe formats: ";
    print_endline "  solve input_file output_file";
    print_endline "  check input_file solutions_file";
    print_endline "  generate num size output_file";
    print_endline "  play input_file output_file";
  end
  else 
    let command = Sys.argv.(1) in
    if command = "play"  
    then
      (* TODO: Implement exception handling if there is no such file *)
      (let input_file = Sys.argv.(2) in
       let output_file = Sys.argv.(3) in
       render_games input_file output_file)                
    else if command = "solve"
    then 
      (let input_file = Sys.argv.(2) in
       let output_file = Sys.argv.(3) in
       let ps = input_file |> file_to_polygons in
       let rooms = List.map polygon_to_room ps in
       let solutions = List.map solve_room_v2 rooms in
       let solution_strings = convert_all_to_strings solutions in
       write_strings_to_file output_file solution_strings)
    else if command = "generate"
    then
      (let num = Sys.argv.(2)
       and size = Sys.argv.(3)
       and output_file = Sys.argv.(4) in
       let room_list = ref [] in
       let i = ref 0 in
       (while !i < (int_of_string num) do
          let random_room = generate_random_room (int_of_string size) in
          if valid_room random_room
          then let () = room_list := List.append !room_list [random_room] in
            i := !i + 1
        done);
       let polygon_list = List.map room_to_polygon !room_list in
       let string_list = List.map polygon_to_string polygon_list in
       write_strings_to_file output_file string_list)
    else if command = "check"
    then 
      (let input_file = Sys.argv.(2) in
       let solution_file = Sys.argv.(3) in
       let rooms = input_file |> file_to_polygons |> (fun x -> List.map polygon_to_room x)
       and solutions = solution_file |> file_to_solutions in
       if List.length rooms = List.length solutions
       then
         (let output = ref [] in 
          for i = 0 to List.length rooms - 1 do
            let sol = List.nth solutions i 
            and number = string_of_int (i + 1) ^ ": " in
            if check_solution (List.nth rooms i) sol
            then output := List.append !output [number ^ string_of_int (List.length sol)] 
            else output := List.append !output [number ^ "Fail"]
          done;
          List.iter print_endline !output)
       else error "Number of rooms do not match number of solutions in check mode"
      )