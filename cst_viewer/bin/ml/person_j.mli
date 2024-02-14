(* Auto-generated from "person.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type nestedName = Person.nestedName = {
  firstName: string;
  lastName: string
}

type person = Person.person = { name: nestedName; age: int }

type constant = Person.constant

val write_nestedName :
  Buffer.t -> nestedName -> unit
  (** Output a JSON value of type {!type:nestedName}. *)

val string_of_nestedName :
  ?len:int -> nestedName -> string
  (** Serialize a value of type {!type:nestedName}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_nestedName :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> nestedName
  (** Input JSON data of type {!type:nestedName}. *)

val nestedName_of_string :
  string -> nestedName
  (** Deserialize JSON data of type {!type:nestedName}. *)

val write_person :
  Buffer.t -> person -> unit
  (** Output a JSON value of type {!type:person}. *)

val string_of_person :
  ?len:int -> person -> string
  (** Serialize a value of type {!type:person}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_person :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> person
  (** Input JSON data of type {!type:person}. *)

val person_of_string :
  string -> person
  (** Deserialize JSON data of type {!type:person}. *)

val write_constant :
  Buffer.t -> constant -> unit
  (** Output a JSON value of type {!type:constant}. *)

val string_of_constant :
  ?len:int -> constant -> string
  (** Serialize a value of type {!type:constant}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_constant :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> constant
  (** Input JSON data of type {!type:constant}. *)

val constant_of_string :
  string -> constant
  (** Deserialize JSON data of type {!type:constant}. *)
