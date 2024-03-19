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


(******************************)
(*  Read a file into a string *)
(******************************)

(* The necessary package *)
open Core

(* Reading *)

let read_file_to_strings filename = 
  let file = In_channel.create filename in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let read_file_to_single_string filename = 
  In_channel.with_file filename ~f:(fun input ->
      In_channel.input_all input)

(* Writing *)

let write_string_to_file filename text = 
  let outc = Out_channel.create ~append:false ~fail_if_exists:false filename in
  Out_channel.output_string outc text;
  Out_channel.close outc

let write_strings_to_file filename lines = 
  Out_channel.with_file ~append:false ~fail_if_exists:false
    filename ~f:(fun out -> List.iter lines ~f:(fun s -> Out_channel.fprintf out "%s\r\n" s))

(* Let's copy a file to another *)

let copy_file old_file new_file = 
  let contents = read_file_to_single_string old_file in
  write_string_to_file new_file contents

(*
Checking consistency of the file

cksum filename
md5 filename

*)

(*************************************)
(*         Character Encodings       *)
(*************************************)

let ascii_string = "ATR"
let utf16_string = "ATЯ"


(* Parsing strings from files *)

let trimmer = Core.String.strip 
    ~drop:(fun c -> Core.List.mem ['\n'; ' '; '\r'] c ~equal:(fun a b -> equal_char a b))

let splitter s = 
  String.split_on_chars ~on:['\n'; ' '; '\r'] s |>
  List.filter ~f:(fun s -> not @@ String.is_empty s)

let my_splitter s = 
  String.split_on_chars ~on:[';'] s |>
  List.filter ~f:(fun s -> not @@ String.is_empty s)

let string_to_pair s =
  let r = Str.regexp {|(\([-0-9]+\), \([-0-9]+\))|} in
  let _ = Str.string_match r s 0 in
  let a = Str.matched_group 1 s
  and b = Str.matched_group 2 s in
  (int_of_string a, int_of_string b)

let print_pair (a, b) = 
  Printf.printf "(%d, %d): " a b

let print_float_pair (a, b) = 
  Printf.printf "(%f, %f)\n" a b


(* let%test "read_file_to_strings" = 
   let ls = read_file_to_strings "/home/zhzchen327/ds_algo_dir/final-project-2022-lilzichen_rinatlamar_renyudogg-1/resources/basic.txt" in
   let s = match ls with
    | [s] ->
      s
    | _ ->
      "nono" in
   let ls' = my_splitter s in
   let ls'' = List.map ls' trimmer in
   let lss = List.map ls'' string_to_pair in
   let () = List.iter lss print_pair in
   true *)

