open Parsetree

(* Type Constant *)

let constant_to_yojson (c : Parsetree.constant) : Yojson.Safe.t =
  match c with
  | Pconst_integer (value, suffix) ->
    `Assoc [
      ("tag", `String "PconstInteger");
      ("contents", `List [
        `String value;
        (match suffix with
        | None -> `Null
        | Some value -> `String (Char.escaped value))
      ])
    ]
  | Pconst_char value ->
    `Assoc [
        ("tag", `String "PconstChar");
        ("contents", `List [
              `Int value
        ])
    ]
  | Pconst_string (value, delim) ->
    `Assoc [
        ("tag", `String "PconstString");
        ("contents", `List [
              `String value;
              (match delim with
              | None -> `Null
              | Some value -> `String value)
        ])
    ]
  | Pconst_float (value, suffix) ->
    `Assoc [
        ("tag", `String "PconstFloat");
        ("contents", `List [
              `String value;
              (match suffix with
              | None -> `Null
              | Some value -> `String (Char.escaped value))
        ])
    ]

(* Type Attribute *)

let rec attribute_to_yojson (a : attribute) : Yojson.Safe.t =
  let (loc, payload) = a in
  `List [
    Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
    payload_to_yojson payload
  ]

(* Type Extension *)

and extension_to_yojson (e : extension) : Yojson.Safe.t =
  let (id, payload) = e in
  `List [
    Asttypes_serializer.loc_to_yojson (fun x -> `String x) id;
    payload_to_yojson payload
  ]

(* Type Attributes *)

and attributes_to_yojson (a : attributes) : Yojson.Safe.t =
  `List (List.map attribute_to_yojson a)

(* Type Payload *)

and structure_to_yojson _s = `String "Structure"
and signature_to_yojson _s = `String "Signature"
and pattern_to_yojson _p = `String "Pattern"
and expression_to_yojson _e = `String "Expression"
and object_field_to_yojson _o = `String "ObjectField"
and closed_flag_to_yojson _cf = `String "ClosedFlag"
and row_field_to_yojson _r = `String "RowField"
and package_type_to_yojson _p = `String "PackageType"

and payload_to_yojson (p : payload) : Yojson.Safe.t =
  match p with
  | PStr s -> `Assoc [
    ("tag", `String "PStr");
    ("contents", `List [structure_to_yojson s])
  ]
  | PSig s -> `Assoc [
    ("tag", `String "PSig");
    ("contents", `List [signature_to_yojson s])
  ]
  | PTyp t -> `Assoc [
    ("tag", `String "PTyp");
    ("contents", `List [core_type_to_yojson t])
  ]
  | PPat (p, e) -> `Assoc [
    ("tag", `String "PPat");
    ("contents", `List [
      pattern_to_yojson p;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]

(* Type CoreType *)

and core_type_to_yojson (t : core_type) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "CoreType");
    ("contents", `List [
      `Assoc [
        ("ptypDesc", core_type_desc_to_yojson t.ptyp_desc);
        ("ptypLoc", Location_serializer.t_to_yojson t.ptyp_loc);
        ("ptypAttributes", attributes_to_yojson t.ptyp_attributes)
      ]
    ])
  ]

(* Type CoreTypeDesc *)

and core_type_desc_to_yojson (t : core_type_desc) : Yojson.Safe.t =
  match t with
  | Ptyp_any -> `Assoc [
    ("tag", `String "PtypAny")
  ]
  | Ptyp_var value -> `Assoc [
    ("tag", `String "PtypVar");
    ("contents", `List [`String value])
  ]
  | Ptyp_arrow (label, t1, t2) -> `Assoc [
    ("tag", `String "PtypArrow");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson label;
      core_type_to_yojson t1;
      core_type_to_yojson t2
    ])
  ]
  | Ptyp_tuple t -> `Assoc [
    ("tag", `String "PtypTuple");
    ("contents", `List [`List (List.map core_type_to_yojson t)])
  ]
  | Ptyp_constr (loc, t) -> `Assoc [
    ("tag", `String "PtypConstr");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson t)
    ])
  ]
  | Ptyp_object (o, cf) -> `Assoc [
    ("tag", `String "PtypObject");
    ("contents", `List [
      `List (List.map object_field_to_yojson o);
      closed_flag_to_yojson cf
    ])
  ]
  | Ptyp_class (loc, t) -> `Assoc [
    ("tag", `String "PtypClass");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson t)
    ])
  ]
  | Ptyp_alias (t, value) -> `Assoc [
    ("tag", `String "PtypAlias");
    ("contents", `List [
      core_type_to_yojson t;
      `String value
    ])
  ]
  | Ptyp_variant (r, cf, l) ->
    `Assoc [
      ("tag", `String "PtypVariant");
      ("contents", `List [
        `List (List.map row_field_to_yojson r);
        closed_flag_to_yojson cf;
        (match l with
        | None -> `Null
        | Some value -> `List (List.map Asttypes_serializer.label_to_yojson value))]
      )
    ]
  | Ptyp_poly (l, t) -> `Assoc [
    ("tag", `String "PtypPoly");
    ("contents", `List [
      `List (List.map (Asttypes_serializer.loc_to_yojson (fun x -> `String x)) l);
      core_type_to_yojson t
    ])
   ]
  | Ptyp_package p -> `Assoc [
    ("tag", `String "PtypPackage");
    ("contents", `List [package_type_to_yojson p])
   ]
  | Ptyp_extension e -> `Assoc [
    ("tag", `String "PtypExtension");
    ("contents", `List [extension_to_yojson e])
   ]
