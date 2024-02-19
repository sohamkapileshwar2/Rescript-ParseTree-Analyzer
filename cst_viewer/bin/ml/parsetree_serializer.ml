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

(* To be removed *)
(* and structure_to_yojson _s = `String "Structure"
and signature_to_yojson _s = `String "Signature"
and pattern_to_yojson _p = `String "Pattern"
and expression_to_yojson _e = `String "Expression"
and object_field_to_yojson _o = `String "ObjectField"
and closed_flag_to_yojson _cf = `String "ClosedFlag"
and row_field_to_yojson _r = `String "RowField"
and package_type_to_yojson _p = `String "PackageType" *)

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
    ("ptypDesc", core_type_desc_to_yojson t.ptyp_desc);
    ("ptypLoc", Location_serializer.t_to_yojson t.ptyp_loc);
    ("ptypAttributes", attributes_to_yojson t.ptyp_attributes)
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

(* Type PackageType *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list

and package_type_to_yojson (p : package_type) : Yojson.Safe.t =
  `List [
    Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson (fst p);
    `List (
      List.map (
        fun (loc, t) -> `List [
          Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
          core_type_to_yojson t
      ]) (snd p)
     )
   ]

(* Type RowField *)

and row_field_to_yojson (r : row_field) : Yojson.Safe.t =
  match r with
  | Rtag (loc, attributes, boolean, core_type_list) -> `Assoc [
    ("tag", `String "Rtag");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      attributes_to_yojson attributes;
      `Bool boolean;
      `List (List.map core_type_to_yojson core_type_list)
    ])
   ]
  | Rinherit t -> `Assoc [
    ("tag", `String "Rinherit");
    ("contents", `List [core_type_to_yojson t])
   ]

(* Type ObjectField *)

and object_field_to_yojson (o : object_field) : Yojson.Safe.t =
  match o with
  | Otag (loc, attributes, core_type) -> `Assoc [
    ("tag", `String "Otag");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      attributes_to_yojson attributes;
      core_type_to_yojson core_type
    ])
  ]
  | Oinherit core_type -> `Assoc [
    ("tag", `String "Oinherit");
    ("contents", `List [core_type_to_yojson core_type])
  ]

(* Type Pattern *)

and pattern_to_yojson (p : pattern) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Pattern");
    ("ppatDesc", pattern_desc_to_yojson p.ppat_desc);
    ("ppatLoc", Location_serializer.t_to_yojson p.ppat_loc);
    ("ppatAttributes", attributes_to_yojson p.ppat_attributes)
  ]

(* Type PatternDesc *)

and pattern_desc_to_yojson (p : pattern_desc) : Yojson.Safe.t =
  match p with
  | Ppat_any -> `Assoc [
    ("tag", `String "PpatAny")
  ]
  | Ppat_var value -> `Assoc [
    ("tag", `String "PpatVar");
    ("contents", `List [Asttypes_serializer.loc_to_yojson (fun x -> `String x) value])
  ]
  | Ppat_alias (p, value) -> `Assoc [
    ("tag", `String "PpatAlias");
    ("contents", `List [
      pattern_to_yojson p;
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) value
    ])
  ]
  | Ppat_constant c -> `Assoc [
    ("tag", `String "PpatConstant");
    ("contents", `List [constant_to_yojson c])
  ]
  | Ppat_interval (c1, c2) -> `Assoc [
    ("tag", `String "PpatInterval");
    ("contents", `List [
      constant_to_yojson c1;
      constant_to_yojson c2
    ])
  ]
  | Ppat_tuple p -> `Assoc [
    ("tag", `String "PpatTuple");
    ("contents", `List [`List (List.map pattern_to_yojson p)])
  ]
  | Ppat_construct (loc, p) -> `Assoc [
    ("tag", `String "PpatConstruct");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      (match p with
      | None -> `Null
      | Some value -> pattern_to_yojson value)
    ])
  ]
  | Ppat_variant (l, p) -> `Assoc [
    ("tag", `String "PpatVariant");
    ("contents", `List [
      Asttypes_serializer.label_to_yojson l;
      (match p with
      | None -> `Null
      | Some value -> pattern_to_yojson value)
    ])
  ]
  | Ppat_record (l, closed_flag) -> `Assoc [
    ("tag", `String "PpatRecord");
    ("contents", `List [
      `List (List.map (fun (loc, p) -> `List [
        Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
        pattern_to_yojson p
      ]) l);
      closed_flag_to_yojson closed_flag
    ])
  | Ppat_array p -> `Assoc [
    ("tag", `String "PpatArray");
    ("contents", `List [`List (List.map pattern_to_yojson p)])
  ]
  | Ppat_or (p1, p2) -> `Assoc [
    ("tag", `String "PpatOr");
    ("contents", `List [pattern_to_yojson p1; pattern_to_yojson p2])
  ]
  | Ppat_constraint (p, t) -> `Assoc [
    ("tag", `String "PpatConstraint");
    ("contents", `List [pattern_to_yojson p; core_type_to_yojson t])
  ]
  | Ppat_type l -> `Assoc [
    ("tag", `String "PpatType");
    ("contents", `List [Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson l])
  ]
  | Ppat_lazy p -> `Assoc [
    ("tag", `String "PpatLazy");
    ("contents", `List [pattern_to_yojson p])
  ]
  | Ppat_unpack value -> `Assoc [
    ("tag", `String "PpatUnpack");
    ("contents", `List [Asttypes_serializer.loc_to_yojson (fun x -> `String x) value])
  ]
  | Ppat_exception p -> `Assoc [
    ("tag", `String "PpatException");
    ("contents", `List [pattern_to_yojson p])
  ]
  | Ppat_extension e -> `Assoc [
    ("tag", `String "PpatExtension");
    ("contents", `List [extension_to_yojson e])
  ]
  | Ppat_open (loc, p) -> `Assoc [
    ("tag", `String "PpatOpen");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      pattern_to_yojson p
    ])
  ]

(* Type Expression *)

and expression_to_yojson (e : expression) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Expression");
    ("pexpDesc", expression_desc_to_yojson e.pexp_desc);
    ("pexpLoc", Location_serializer.t_to_yojson e.pexp_loc);
    ("pexpAttributes", attributes_to_yojson e.pexp_attributes)
  ]

(* Type ExpressionDesc *)

and expression_desc_to_yojson (e : expression_desc) : Yojson.Safe.t =
  match e with
  | Pexp_ident loc -> `Assoc [
    ("tag", `String "PexpIdent");
    ("contents", `List [Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc])
  ]
  | Pexp_constant c -> `Assoc [
    ("tag", `String "PexpConstant");
    ("contents", `List [constant_to_yojson c])
  ]
  | Pexp_let (rf, v, e) -> `Assoc [
    ("tag", `String "PexpLet");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map value_binding_to_yojson v);
      expression_to_yojson e
    ])
  ]
  | Pexp_function c -> `Assoc [
    ("tag", `String "PexpFunction");
    ("contents", `List [`List (List.map case_to_yojson c)]
  ]
  | Pexp_fun (l, d, p, e) -> `Assoc [
    ("tag", `String "PexpFun");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson l;
      (match d with
      | None -> `Null
      | Some value -> expression_to_yojson value);
      pattern_to_yojson p;
      expression_to_yojson e
    ])
  ]
  | Pexp_apply (e, l) -> `Assoc [
    ("tag", `String "PexpApply");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map (fun (l, e) -> `List [
        Asttypes_serializer.arg_label_to_yojson l;
        expression_to_yojson e
      ]) l)
    ])
  ]
  | Pexp_match (e, c) -> `Assoc [
    ("tag", `String "PexpMatch");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map case_to_yojson c)
    ])
  ]
  | Pexp_try (e, c) -> `Assoc [
    ("tag", `String "PexpTry");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map case_to_yojson c)
    ])
  | Pexp_tuple l -> `Assoc [
    ("tag", `String "PexpTuple");
    ("contents", `List [`List (List.map expression_to_yojson l)])
  ]
  | Pexp_construct (loc, e) -> `Assoc [
    ("tag", `String "PexpConstruct");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_variant (l, e) -> `Assoc [
    ("tag", `String "PexpVariant");
    ("contents", `List [
      Asttypes_serializer.label_to_yojson l;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_record (l, e) -> `Assoc [
    ("tag", `String "PexpRecord");
    ("contents", `List [
      `List (List.map (fun (loc, e) -> `List [
        Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
        expression_to_yojson e
      ]) l);
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  | Pexp_field (e, loc) -> `Assoc [
    ("tag", `String "PexpField");
    ("contents", `List [
      expression_to_yojson e;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc
    ])
  ]
  | Pexp_setfield (e1, loc, e2) -> `Assoc [
    ("tag", `String "PexpSetField");
    ("contents", `List [
      expression_to_yojson e1;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      expression_to_yojson e2
    ])
  ]
  | Pexp_array l -> `Assoc [
    ("tag", `String "PexpArray");
    ("contents", `List [`List (List.map expression_to_yojson l)])
  ]
  | Pexp_ifthenelse (e1, e2, e3) -> `Assoc [
    ("tag", `String "PexpIfThenElse");
    ("contents", `List [
      expression_to_yojson e1;
      expression_to_yojson e2;
      (match e3 with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_sequence (e1, e2) -> `Assoc [
    ("tag", `String "PexpSequence");
    ("contents", `List [expression_to_yojson e1; expression_to_yojson e2])
  ]
  | Pexp_while (e1, e2) -> `Assoc [
    ("tag", `String "PexpWhile");
    ("contents", `List [expression_to_yojson e1; expression_to_yojson e2])
  ]
  | Pexp_for (p, e1, e2, d, e3) -> `Assoc [
    ("tag", `String "PexpFor");
    ("contents", `List [
      pattern_to_yojson p;
      expression_to_yojson e1;
      expression_to_yojson e2;
      Asttypes_serializer.direction_flag_to_yojson d;
      expression_to_yojson e3
    ])
  ]
  | Pexp_constraint (e, t) -> `Assoc [
    ("tag", `String "PexpConstraint");
    ("contents", `List [expression_to_yojson e; core_type_to_yojson t])
  ]
  | Pexp_coerce (e, t1, t2) -> `Assoc [
    ("tag", `String "PexpCoerce");
    ("contents", `List [
      expression_to_yojson e;
      match t1 with None -> `Null | Some t -> core_type_to_yojson t;
      core_type_to_yojson t2
    ])
  ]
  | Pexp_send (e, l) -> `Assoc [
    ("tag", `String "PexpSend");
    ("contents", `List [
      expression_to_yojson e;
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson l
    ])
  ]
  | Pexp_new loc -> `Assoc [
    ("tag", `String "PexpNew");
    ("contents", `List [Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc])
  ]
  | Pset_instvar (loc, e) -> `Assoc [
    ("tag", `String "PsetInstvar");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson label_to_yojson loc;
      expression_to_yojson e
    ])
  ]
  | Pexp_override l -> `Assoc [
    ("tag", `String "PexpOverride");
    ("contents", `List [
      `List (List.map (fun (loc, e) -> `List [
        Asttypes_serializer.loc_to_yojson label_to_yojson loc;
        expression_to_yojson e
      ]) l)
    ])
  ]
  | Pexp_letmodule (loc, e1, e2) -> `Assoc [
    ("tag", `String "PexpLetModule");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      module_expr_to_yojson e1;
      expression_to_yojson e2
    ])
  ]
  | Pexp_letexception (ec, e) -> `Assoc [
    ("tag", `String "PexpLetException");
    ("contents", `List [
      extension_constructor_to_yojson ec;
      expression_to_yojson e
    ])
  ]
  | Pexp_assert e -> `Assoc [
    ("tag", `String "PexpAssert");
    ("contents", `List [expression_to_yojson e])
  ]
  | Pexp_lazy e -> `Assoc [
    ("tag", `String "PexpLazy");
    ("contents", `List [expression_to_yojson e])
  ]
  | Pexp_poly (e, t) -> `Assoc [
    ("tag", `String "PexpPoly");
    ("contents", `List [
      expression_to_yojson e; 
      (match t with None -> `Null | Some t -> core_type_to_yojson t)
    ])
  ]
  | Pexp_object o -> `Assoc [
    ("tag", `String "PexpObject");
    ("contents", `List [class_structure_to_yojson o])
  ]
  | Pexp_newtype (loc, e) -> `Assoc [
    ("tag", `String "PexpNewtype");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      expression_to_yojson e
    ])
  ]
  | 