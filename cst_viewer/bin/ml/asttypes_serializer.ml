open Asttypes

(* Type Loc *)

let loc_to_yojson (a_to_yojson : 'a -> Yojson.Safe.t) (loc : 'a loc) : Yojson.Safe.t =
  `Assoc [
      ("tag", `String "Loc");
      ("contents", `List [
        `Assoc [
          ("txt", a_to_yojson loc.txt);
          ("loc", Location_serializer.t_to_yojson loc.loc)
        ]
      ])
    ]

let arg_label_to_yojson (arg_label : arg_label) : Yojson.Safe.t =
  (match arg_label with
    | Nolabel ->
      `Assoc [
        ("tag", `String "Nolabel")
      ]
    | Labelled s ->
      `Assoc [
        ("tag", `String "Labelled");
        ("contents", `String s)
      ]
    | Optional s ->
      `Assoc [
        ("tag", `String "Optional");
        ("contents", `String s)
      ]
  )

let label_to_yojson (label : label) : Yojson.Safe.t = `String label