(* (* open Res_driver
open Parsetree
open Yojson
(* open Yojson.Basic *)

let store_person_as_json_file filename person =
  let json_str = Yojson.Safe.to_string (Parsetree.structure_to_yojson person) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc

let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  store_person_as_json_file "res.json" parseResult.parsetree
  

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <rescript_file>\n" Sys.argv.(0);
    exit 1
  );
  let file_name = Sys.argv.(1) in
  Printf.eprintf "Filename: %s \n" file_name;
  (* let parsed_ast = parse_file file_name in *)
  (* List.iter print_structure_item parsed_ast *)
  parse_file file_name *)


(* Define an OCaml type *)
(* open Yojson *)

(* Define an OCaml type *)

(* Define a nested type *)

(* open Person_j *)
open Person
open Parsetree
open Parsetree_serializer
open Res_driver

let my_name = { firstName = "Alice"; lastName = "Smith" }
let _my_person =
  { name = my_name;
    age =
      match Some 5 with
      | None -> 10
      | Some u -> u
  }

let _my_constant = Pconst_float ("5.0", Some 'f')

let _my_attribute : attribute = ({
    txt : string = "Hello";
    loc = {
      loc_start = {pos_fname = "file"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
      loc_end = {pos_fname = "file"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
      loc_ghost = false
    }
  }, PSig [])

let store_person_as_json_file filename result =
  let json_str = Yojson.Safe.to_string (structure_to_yojson result) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc

(* Usage *)
(* let () = store_person_as_json_file "person.json" my_person *)


let parse_file file_name =
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  store_person_as_json_file "../haskell_rescript_types/res.json" parseResult.parsetree

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <rescript_file>\n" Sys.argv.(0);
    exit 1
  );
  let file_name = Sys.argv.(1) in
  Printf.eprintf "Filename: %s \n" file_name;
  (* let parsed_ast = parse_file file_name in *)
  (* List.iter print_structure_item parsed_ast *)
  parse_file file_name *)

open Res_driver

let _store_string_as_file filename str _constant =
  let json_str = str in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc


let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  parseResult

let () =
  let parsetree = parse_file "sample_rescript.res" in
  Res_driver.printEngine.printImplementation
    ~width:4
    ~filename:"xxx.res"
    ~comments:[]
    parsetree.parsetree;
  print_endline "Done!"