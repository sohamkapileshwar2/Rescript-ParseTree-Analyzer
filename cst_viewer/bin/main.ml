(* open Res_driver
(* open Yojson.Basic *)

let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let _parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  print_endline ("string_of_int parseResult.parsetree")

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
open Yojson.Safe

(* Define an OCaml type *)
type person = {
  name : string;
  age : int;
  } 
[@@deriving to_yojson]

(* Example person *)
let my_person = { name = "Alice"; age = 30 }
(* let xx = person_to_yojson my_person *)

(* Convert person to JSON and store in a file *)
let store_person_as_json_file filename person =
  let json_str = Yojson.Safe.to_string (person_to_yojson person) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc

(* Usage *)
let () = store_person_as_json_file "person.json" my_person


