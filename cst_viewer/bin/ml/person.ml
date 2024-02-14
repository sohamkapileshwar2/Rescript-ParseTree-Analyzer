type nestedName = {
  firstName : string;
  lastName : string;
}

let nestedName_to_yojson (nestedName : nestedName) : Yojson.Safe.t =
  `Assoc [
    ("firstName", `String nestedName.firstName);
    ("lastName", `String nestedName.lastName);
  ]

type person = {
  name : nestedName;
  age : int;
  }

let person_to_yojson (person : person) : Yojson.Safe.t =
  `Assoc [
    ("name", nestedName_to_yojson person.name);
    ("age", `Int person.age);
  ]

type constant =
  Pconst_integer of string * string option
  (* 3 3l 3L 3n

    Suffixes [g-z][G-Z] are accepted by the parser.
    Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of int
  (* 'c' *)
  | Pconst_string of string * string option
  (* "constant"
    {delim|other constant|delim}
  *)
  | Pconst_float of string * string option
  (* 3.4 2e5 1.4e-4

    Suffixes [g-z][G-Z] are accepted by the parser.
    Suffixes are rejected by the typechecker.
  *)
  [@@deriving to_yojson]