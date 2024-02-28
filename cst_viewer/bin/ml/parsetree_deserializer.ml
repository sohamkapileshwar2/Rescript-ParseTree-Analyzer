(* open Parsetree


let constant_of_yojson (json : Yojson.Safe.t) : Parsetree.constant =
  match json with
  | `Assoc [("tag", `String "PconstInteger"); ("contents", `List [`String value; suffix])] ->
    let suffix_value =
      match suffix with
      | `Null -> None
      | `String value -> Some (Char.chr (int_of_string value))
      | _ -> failwith "Invalid JSON format for suffix of PconstInteger"
    in
    Pconst_integer (value, suffix_value)
  | `Assoc [("tag", `String "PconstChar"); ("contents", `Int value)] -> Pconst_char value
  | `Assoc [("tag", `String "PconstString"); ("contents", `List [`String value; delim])] ->
    let delim_value =
      match delim with
      | `Null -> None
      | `String value -> Some value
      | _ -> failwith "Invalid JSON format for delimiter of PconstString"
    in
    Pconst_string (value, delim_value)
  | `Assoc [("tag", `String "PconstFloat"); ("contents", `List [`String value; suffix])] ->
    let suffix_value =
      match suffix with
      | `Null -> None
      | `String value -> Some (Char.chr (int_of_string value))
      | _ -> failwith "Invalid JSON format for suffix of PconstFloat"
    in
    Pconst_float (value, suffix_value)
  | _ -> failwith "Invalid JSON format for Parsetree.constant"


let rec attribute_of_yojson (json : Yojson.Safe.t) : attribute =
  match json with
  | `List [loc_json; payload_json] ->
    let loc = Asttypes_deserializer.loc_of_yojson loc_json in
    let payload = payload_of_yojson payload_json in
    (loc, payload)
  | _ -> failwith "Invalid JSON format for attribute"


and extension_of_yojson (json : Yojson.Safe.t) : extension =
  match json with
  | `List [loc_json; payload_json] ->
    let loc = Asttypes_deserializer.loc_of_yojson loc_json in
    let payload = payload_of_yojson payload_json in
    (loc, payload)
  | _ -> failwith "Invalid JSON format for extension"

and attributes_of_yojson (json : Yojson.Safe.t) : attributes =
  match json with
  | `List attributes_json ->
    List.map attribute_of_yojson attributes_json
  | _ -> failwith "Invalid JSON format for attributes"


let payload_of_yojson (json : Yojson.Safe.t) : payload =
  match json with
  | `Assoc [("tag", `String tag); ("contents", contents_json)] ->
    (match tag with
    | "PStr" -> PStr (structure_of_yojson contents_json)
    | "PSig" -> PSig (signature_of_yojson contents_json)
    | "PTyp" -> PTyp (core_type_of_yojson contents_json)
    | "PPat" -> 
      let contents = Yojson.Safe.Util.to_list contents_json in
      (match contents with
      | [pattern_json; expression_json] ->
        let pattern = pattern_of_yojson pattern_json in
        let expression = 
          match expression_json with
          | `Null -> None
          | _ -> Some (expression_of_yojson expression_json)
        in
        PPat (pattern, expression)
      | _ -> failwith "Invalid JSON format for PPat")
    | _ -> failwith "Invalid tag value")
  | _ -> failwith "Invalid JSON format for payload"


let core_type_of_yojson (json : Yojson.Safe.t) : core_type =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    if tag = "CoreType" then begin
      let desc_json = Yojson.Safe.Util.(member "ptypDesc" fields) in
      let loc_json = Yojson.Safe.Util.(member "ptypLoc" fields) in
      let attributes_json = Yojson.Safe.Util.(member "ptypAttributes" fields) in
      let desc = core_type_desc_of_yojson desc_json in
      let loc = Location_serializer.t_of_yojson loc_json in
      let attributes = attributes_of_yojson attributes_json in
      { ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attributes }
    end else
      failwith "Invalid tag value"
  | _ -> failwith "Invalid JSON format for core_type"

let core_type_desc_of_yojson (json : Yojson.Safe.t) : core_type_desc =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    begin match tag with
    | "PtypAny" -> Ptyp_any
    | "PtypVar" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_string) in
      Ptyp_var contents
    | "PtypArrow" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list |> List.map core_type_of_yojson) in
      (match contents with
      | [arg_label_json; t1_json; t2_json] ->
        let label = Asttypes_deserializer.arg_label_of_yojson arg_label_json in
        let t1 = core_type_of_yojson t1_json in
        let t2 = core_type_of_yojson t2_json in
        Ptyp_arrow (label, t1, t2)
      | _ -> failwith "Invalid JSON format for PtypArrow")
    | "PtypTuple" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list |> List.map core_type_of_yojson) in
      Ptyp_tuple contents
    | "PtypConstr" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; types_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let types = List.map core_type_of_yojson types_json in
        Ptyp_constr (loc, types)
      | _ -> failwith "Invalid JSON format for PtypConstr")
    | "PtypObject" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [fields_json; closed_flag_json] ->
        let fields = List.map object_field_of_yojson (Yojson.Safe.Util.to_list fields_json) in
        let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag_json in
        Ptyp_object (fields, closed_flag)
      | _ -> failwith "Invalid JSON format for PtypObject")
    | "PtypClass" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; types_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let types = List.map core_type_of_yojson types_json in
        Ptyp_class (loc, types)
      | _ -> failwith "Invalid JSON format for PtypClass")
    | "PtypAlias" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [t_json; value] ->
        let t = core_type_of_yojson t_json in
        Ptyp_alias (t, value)
      | _ -> failwith "Invalid JSON format for PtypAlias")
    | "PtypVariant" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [row_fields_json; closed_flag_json; labels_json] ->
        let row_fields = List.map row_field_of_yojson (Yojson.Safe.Util.to_list row_fields_json) in
        let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag_json in
        let labels = 
          match labels_json with
          | `Null -> None
          | _ -> Some (List.map Asttypes_deserializer.label_of_yojson (Yojson.Safe.Util.to_list labels_json))
        in
        Ptyp_variant (row_fields, closed_flag, labels)
      | _ -> failwith "Invalid JSON format for PtypVariant")
    | "PtypPoly" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [locs_json; t_json] ->
        let locs = List.map (fun x -> Asttypes_deserializer.loc_of_yojson x |> Yojson.Safe.Util.to_string) (Yojson.Safe.Util.to_list locs_json) in
        let t = core_type_of_yojson t_json in
        Ptyp_poly (locs, t)
      | _ -> failwith "Invalid JSON format for PtypPoly")
    | "PtypPackage" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Ptyp_package (package_type_of_yojson contents)
    | "PtypExtension" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Ptyp_extension (extension_of_yojson contents)
    | _ -> failwith "Invalid tag value"
    end
  | _ -> failwith "Invalid JSON format for core_type_desc"


and package_type_of_yojson (json : Yojson.Safe.t) : package_type =
  match json with
  | `List [loc_json; items_json] ->
    let loc = Asttypes_deserializer.loc_of_yojson loc_json in
    let items = 
      match items_json with
      | `List l ->
        List.map (fun item ->
          match item with
          | `List [loc_item_json; core_type_json] ->
            let loc_item = Asttypes_deserializer.loc_of_yojson loc_item_json in
            let core_type = core_type_of_yojson core_type_json in
            (loc_item, core_type)
          | _ -> failwith "Invalid JSON format for package_type item"
        ) l
      | _ -> failwith "Invalid JSON format for package_type items"
    in
    (loc, items)
  | _ -> failwith "Invalid JSON format for package_type"

and row_field_of_yojson (json : Yojson.Safe.t) : row_field =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    begin match tag with
    | "Rtag" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; attributes_json; boolean_json; core_type_list_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let attributes = attributes_of_yojson attributes_json in
        let boolean = Yojson.Safe.Util.(to_bool boolean_json) in
        let core_type_list = List.map core_type_of_yojson (Yojson.Safe.Util.to_list core_type_list_json) in
        Rtag (loc, attributes, boolean, core_type_list)
      | _ -> failwith "Invalid JSON format for Rtag")
    | "Rinherit" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Rinherit (core_type_of_yojson contents)
    | _ -> failwith "Invalid tag value"
    end
  | _ -> failwith "Invalid JSON format for row_field"

and object_field_of_yojson (json : Yojson.Safe.t) : object_field =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    begin match tag with
    | "Otag" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; attributes_json; core_type_json] ->
        let loc = Asttypes_serializer.loc_of_yojson loc_json in
        let attributes = attributes_of_yojson attributes_json in
        let core_type = core_type_of_yojson core_type_json in
        Otag (loc, attributes, core_type)
      | _ -> failwith "Invalid JSON format for Otag")
    | "Oinherit" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Oinherit (core_type_of_yojson contents)
    | _ -> failwith "Invalid tag value"
    end
  | _ -> failwith "Invalid JSON format for object_field"

and pattern_of_yojson (json : Yojson.Safe.t) : pattern =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    if tag = "Pattern" then begin
      let desc_json = Yojson.Safe.Util.(member "ppatDesc" fields) in
      let loc_json = Yojson.Safe.Util.(member "ppatLoc" fields) in
      let attributes_json = Yojson.Safe.Util.(member "ppatAttributes" fields) in
      let desc = pattern_desc_of_yojson desc_json in
      let loc = Location_deserializer.t_of_yojson loc_json in
      let attributes = attributes_of_yojson attributes_json in
      { ppat_desc = desc; ppat_loc = loc; ppat_attributes = attributes }
    end else
      failwith "Invalid tag value"
  | _ -> failwith "Invalid JSON format for pattern"


and pattern_desc_of_yojson (json : Yojson.Safe.t) : pattern_desc =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    begin match tag with
    | "PpatAny" -> Ppat_any
    | "PpatVar" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let value = Asttypes_deserializer.loc_of_yojson contents |> Asttypes_serializer.to_string in
      Ppat_var value
    | "PpatAlias" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [p_json; value_json] ->
        let p = pattern_of_yojson p_json in
        let value = Asttypes_deserializer.loc_of_yojson value_json |> Asttypes_serializer.to_string in
        Ppat_alias (p, value)
      | _ -> failwith "Invalid JSON format for PpatAlias")
    | "PpatConstant" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Ppat_constant (constant_of_yojson contents)
    | "PpatInterval" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [c1_json; c2_json] ->
        let c1 = constant_of_yojson c1_json in
        let c2 = constant_of_yojson c2_json in
        Ppat_interval (c1, c2)
      | _ -> failwith "Invalid JSON format for PpatInterval")
    | "PpatTuple" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let p_list = List.map pattern_of_yojson contents in
      Ppat_tuple p_list
    | "PpatConstruct" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; p_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let p = 
          match p_json with
          | `Null -> None
          | _ -> Some (pattern_of_yojson p_json)
        in
        Ppat_construct (loc, p)
      | _ -> failwith "Invalid JSON format for PpatConstruct")
    | "PpatVariant" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [label_json; p_json] ->
        let label = Asttypes_deserializer.label_of_yojson label_json in
        let p = 
          match p_json with
          | `Null -> None
          | _ -> Some (pattern_of_yojson p_json)
        in
        Ppat_variant (label, p)
      | _ -> failwith "Invalid JSON format for PpatVariant")
    | "PpatRecord" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [record_json; closed_flag_json] ->
        let record_list = List.map (fun item ->
          match item with
          | `List [loc_json; p_json] ->
            let loc = Asttypes_deserializer.loc_of_yojson loc_json in
            let p = pattern_of_yojson p_json in
            (loc, p)
          | _ -> failwith "Invalid JSON format for PpatRecord item"
        ) (Yojson.Safe.Util.to_list record_json) in
        let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag_json in
        Ppat_record (record_list, closed_flag)
      | _ -> failwith "Invalid JSON format for PpatRecord")
    | "PpatArray" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let p_list = List.map pattern_of_yojson contents in
      Ppat_array p_list
    | "PpatOr" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [p1_json; p2_json] ->
        let p1 = pattern_of_yojson p1_json in
        let p2 = pattern_of_yojson p2_json in
        Ppat_or (p1, p2)
      | _ -> failwith "Invalid JSON format for PpatOr")
    | "PpatConstraint" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [p_json; t_json] ->
        let p = pattern_of_yojson p_json in
        let t = core_type_of_yojson t_json in
        Ppat_constraint (p, t)
      | _ -> failwith "Invalid JSON format for PpatConstraint")
    | "PpatType" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let l = Asttypes_deserializer.loc_of_yojson contents in
      Ppat_type l
    | "PpatLazy" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let p = pattern_of_yojson contents in
      Ppat_lazy p
    | "PpatUnpack" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let value = Asttypes_deserializer.loc_of_yojson contents |> Asttypes_serializer.to_string in
      Ppat_unpack value
    | "PpatException" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let p = pattern_of_yojson contents in
      Ppat_exception p
    | "PpatExtension" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Ppat_extension (extension_of_yojson contents)
    | "PpatOpen" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; p_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let p = pattern_of_yojson p_json in
        Ppat_open (loc, p)
      | _ -> failwith "Invalid JSON format for PpatOpen")
    | _ -> failwith "Invalid tag value"
    end
  | _ -> failwith "Invalid JSON format for pattern_desc"


and expression_of_yojson (json : Yojson.Safe.t) : expression =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    if tag = "Expression" then begin
      let desc_json = Yojson.Safe.Util.(member "pexpDesc" fields) in
      let loc_json = Yojson.Safe.Util.(member "pexpLoc" fields) in
      let attributes_json = Yojson.Safe.Util.(member "pexpAttributes" fields) in
      let desc = expression_desc_of_yojson desc_json in
      let loc = Location_deserializer.t_of_yojson loc_json in
      let attributes = attributes_of_yojson attributes_json in
      { pexp_desc = desc; pexp_loc = loc; pexp_attributes = attributes }
    end else
      failwith "Invalid tag value"
  | _ -> failwith "Invalid JSON format for expression"


and expression_desc_of_yojson (json : Yojson.Safe.t) : expression_desc =
  match json with
  | `Assoc fields ->
    let tag = Yojson.Safe.Util.(member "tag" json |> to_string) in
    begin match tag with
    | "PexpIdent" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let l = Longident_deserializer.t_of_yojson contents in
      Pexp_ident l
    | "PexpConstant" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Pexp_constant (constant_of_yojson contents)
    | "PexpLet" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [rec_flag_json; value_bindings_json; expression_json] ->
        let rec_flag = Asttypes_deserializer.rec_flag_of_yojson rec_flag_json in
        let value_bindings = List.map value_binding_of_yojson (Yojson.Safe.Util.to_list value_bindings_json) in
        let expression = expression_of_yojson expression_json in
        Pexp_let (rec_flag, value_bindings, expression)
      | _ -> failwith "Invalid JSON format for PexpLet")
    | "PexpFunction" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let cases = List.map case_of_yojson contents in
      Pexp_function cases
    | "PexpFun" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [label_json; default_json; pattern_json; expression_json] ->
        let label = Asttypes_deserializer.arg_label_of_yojson label_json in
        let default = expression_of_yojson default_json in
        let pattern = pattern_of_yojson pattern_json in
        let expression = expression_of_yojson expression_json in
        Pexp_fun (label, default, pattern, expression)
      | _ -> failwith "Invalid JSON format for PexpFun")
    | "PexpFunction" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let cases = List.map case_of_yojson contents in
      Pexp_function cases
    | "PexpApply" -> 
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [f_json; args_json] ->
        let f = expression_of_yojson f_json in
        let args = List.map (fun x -> expression_of_yojson x) (Yojson.Safe.Util.to_list args_json) in
        Pexp_apply (f, args)
      | _ -> failwith "Invalid JSON format for PexpApply")
    | "PexpMatch" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression_json; cases_json] ->
        let expression = expression_of_yojson expression_json in
        let cases = List.map case_of_yojson (Yojson.Safe.Util.to_list cases_json) in
        Pexp_match (expression, cases)
      | _ -> failwith "Invalid JSON format for PexpMatch")
    | "PexpTry" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression_json; cases_json] ->
        let expression = expression_of_yojson expression_json in
        let cases = List.map case_of_yojson (Yojson.Safe.Util.to_list cases_json) in
        Pexp_try (expression, cases)
      | _ -> failwith "Invalid JSON format for PexpTry")
    | "PexpTuple" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let expressions = List.map expression_of_yojson (Yojson.Safe.Util.to_list contents) in
      Pexp_tuple expressions
    | "PexpConstruct" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; expression_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let expression = 
          match expression_json with
          | `Null -> None
          | _ -> Some (expression_of_yojson expression_json)
        in
        Pexp_construct (loc, expression)
      | _ -> failwith "Invalid JSON format for PexpConstruct")
    | "PexpVariant" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [label_json; expression_json] ->
        let label = Asttypes_deserializer.label_of_yojson label_json in
        let expression = 
          match expression_json with
          | `Null -> None
          | _ -> Some (expression_of_yojson expression_json)
        in
        Pexp_variant (label, expression)
      | _ -> failwith "Invalid JSON format for PexpVariant")
    | "PexpRecord" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [record_json; expression_json] ->
        let record_list = List.map (fun item ->
          match item with
          | `List [loc_json; expression_json] ->
            let loc = Asttypes_deserializer.loc_of_yojson loc_json in
            let expression = expression_of_yojson expression_json in
            (loc, expression)
          | _ -> failwith "Invalid JSON format for PexpRecord item"
        ) (Yojson.Safe.Util.to_list record_json) in
        let expression = 
          match expression_json with
          | `Null -> None
          | _ -> Some (expression_of_yojson expression_json)
        in
        Pexp_record (record_list, expression)
      | _ -> failwith "Invalid JSON format for PexpRecord")
    | "PexpField" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression_json; loc_json] ->
        let expression = expression_of_yojson expression_json in
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        Pexp_field (expression, loc)
      | _ -> failwith "Invalid JSON format for PexpField")
    | "PexpSetField" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression1_json; loc_json; expression2_json] ->
        let expression1 = expression_of_yojson expression1_json in
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let expression2 = expression_of_yojson expression2_json in
        Pexp_setfield (expression1, loc, expression2)
      | _ -> failwith "Invalid JSON format for PexpSetField")
    | "PexpArray" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let expressions = List.map expression_of_yojson (Yojson.Safe.Util.to_list contents) in
      Pexp_array expressions
    | "PexpIfthenelse" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression1_json; expression2_json; expression3_json] ->
        let expression1 = expression_of_yojson expression1_json in
        let expression2 = expression_of_yojson expression2_json in
        let expression3 = 
          match expression3_json with
          | `Null -> None
          | _ -> Some (expression_of_yojson expression3_json)
        in
        Pexp_ifthenelse (expression1, expression2, expression3)
      | _ -> failwith "Invalid JSON format for PexpIfthenelse")
    | "PexpSequence" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let expressions = List.map expression_of_yojson (Yojson.Safe.Util.to_list contents) in
      Pexp_sequence expressions
    | "PexpWhile" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression1_json; expression2_json] ->
        let expression1 = expression_of_yojson expression1_json in
        let expression2 = expression_of_yojson expression2_json in
        Pexp_while (expression1, expression2)
      | _ -> failwith "Invalid JSON format for PexpWhile")
    | "PexpFor" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [pattern_json; expression1_json; expression2_json; direction_json; expression3_json] ->
        let pattern = pattern_of_yojson pattern_json in
        let expression1 = expression_of_yojson expression1_json in
        let expression2 = expression_of_yojson expression2_json in
        let direction = Asttypes_deserializer.direction_flag_of_yojson direction_json in
        let expression3 = expression_of_yojson expression3_json in
        Pexp_for (pattern, expression1, expression2, direction, expression3)
      | _ -> failwith "Invalid JSON format for PexpFor")
    | "PexpConstraint" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression_json; core_type_json] ->
        let expression = expression_of_yojson expression_json in
        let core_type = core_type_of_yojson core_type_json in
        Pexp_constraint (expression, core_type)
      | _ -> failwith "Invalid JSON format for PexpConstraint")
    | "PexpCoerce" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression_json; core_type1_json; core_type2_json] ->
        let expression = expression_of_yojson expression_json in
        let core_type1 = core_type_of_yojson core_type1_json in
        let core_type2 = core_type_of_yojson core_type2_json in
        Pexp_coerce (expression, core_type1, core_type2)
      | _ -> failwith "Invalid JSON format for PexpCoerce")
    | "PexpSend" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [expression1_json; loc_json; expression2_json] ->
        let expression1 = expression_of_yojson expression1_json in
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let expression2 = expression_of_yojson expression2_json in
        Pexp_send (expression1, loc, expression2)
      | _ -> failwith "Invalid JSON format for PexpSend")
    | "PexpNew" ->
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      let l = Longident_deserializer.t_of_yojson contents in
      Pexp_new l
    | "PexpSetinstvar" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; expression_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let expression = expression_of_yojson expression_json in
        Pexp_setinstvar (loc, expression)
      | _ -> failwith "Invalid JSON format for PexpSetinstvar")
    | "PexpOverride" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      let bindings = List.map (fun item ->
        match item with
        | `List [loc_json; expression_json] ->
          let loc = Asttypes_deserializer.loc_of_yojson loc_json in
          let expression = expression_of_yojson expression_json in
          (loc, expression)
        | _ -> failwith "Invalid JSON format for PexpOverride item"
      ) (Yojson.Safe.Util.to_list contents) in
      Pexp_override bindings
    | "PexpLetmodule" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [loc_json; module_expr_json; expression_json] ->
        let loc = Asttypes_deserializer.loc_of_yojson loc_json in
        let module_expr = module_expr_of_yojson module_expr_json in
        let expression = expression_of_yojson expression_json in
        Pexp_letmodule (loc, module_expr, expression)
      | _ -> failwith "Invalid JSON format for PexpLetmodule")
    | "PexpLetException" ->
      let contents = Yojson.Safe.Util.(member "contents" fields |> to_list) in
      (match contents with
      | [extension_constructor_json; expression_json] ->
        let extension_constructor = extension_constructor_of_yojson extension_constructor_json in
        let expression = expression_of_yojson expression_json in
        Pexp_letexception (extension_constructor, expression)
      | _ -> failwith "Invalid JSON format for PexpLetexception")
    | "PexpObject" ->
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Pexp_object (class_structure_of_yojson contents)
    | "PexpPack" ->
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Pexp_pack (module_expr_of_yojson contents)
    | "PexpUnreachable" -> Pexp_unreachable
    | "PexpExtension" ->
      let contents = Yojson.Safe.Util.(member "contents" fields) in
      Pexp_extension (extension_of_yojson contents)
    | _ -> failwith "Invalid tag value"
    end
  | _ -> failwith "Invalid JSON format for expression_desc" *)