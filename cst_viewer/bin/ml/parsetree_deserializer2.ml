open Parsetree

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
  | `List jsons -> List.map attribute_of_yojson jsons
  | _ -> failwith "Invalid JSON format for attributes"

and payload_of_yojson (json : Yojson.Safe.t) : payload =
  match json with
  | `Assoc [("tag", `String "PStr"); ("contents", jsons)] ->
    PStr (structure_of_yojson jsons)
  | `Assoc [("tag", `String "PSig"); ("contents", jsons)] ->
    PSig (signature_of_yojson jsons)
  | `Assoc [("tag", `String "PTyp"); ("contents", json)] ->
    PTyp (core_type_of_yojson json)
  | `Assoc [("tag", `String "PPat"); ("contents", json)] ->
    (match json with
     | `List [pat_json; option_expr_json] ->
       let pat = pattern_of_yojson pat_json in
       let option_expr =
         match option_expr_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson option_expr_json)
       in
       PPat (pat, option_expr)
     | _ -> failwith "Invalid JSON format for PPat")
  | _ -> failwith "Invalid JSON format for payload"

and core_type_of_yojson (json : Yojson.Safe.t) : core_type =
  match json with
  | `Assoc fields ->
    {
      ptyp_desc = Yojson.Safe.Util.(`Assoc fields |> member "ptypDesc" |> core_type_desc_of_yojson);
      ptyp_loc = Yojson.Safe.Util.(`Assoc fields |> member "ptypLoc" |> Location_deserializer.t_of_yojson);
      ptyp_attributes = Yojson.Safe.Util.(`Assoc fields |> member "ptypAttributes" |> attributes_of_yojson);
    }
  | _ -> failwith "Invalid JSON format for core_type"

and core_type_desc_of_yojson (json : Yojson.Safe.t) : core_type_desc = 
  match json with
  | `Assoc [("tag", `String "PtypAny")] -> Ptyp_any
  | `Assoc [("tag", `String "PtypVar"); ("contents", `String value)] -> Ptyp_var value
  | `Assoc [("tag", `String "PtypArrow"); ("contents", jsons)] ->
    (match jsons with
     | `List [label; core_type1_json; core_type2_json] ->
       let label = label_of_yojson label in
       let core_type1 = core_type_of_yojson core_type1_json in
       let core_type2 = core_type_of_yojson core_type2_json in
       Ptyp_arrow (label, core_type1, core_type2)
     | _ -> failwith "Invalid JSON format for Ptyp_arrow")
  | `Assoc [("tag", `String "PtypTuple"); ("contents", jsons)] ->
    (match jsons with
     | `List core_types_json -> Ptyp_tuple (List.map core_type_of_yojson core_types_json)
     | _ -> failwith "Invalid JSON format for Ptyp_tuple")
  | `Assoc [("tag", `String "PtypConstr"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; (`List core_types_json)] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Ptyp_constr (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Ptyp_constr")
  | `Assoc [("tag", `String "PtypObject"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List fields); closed_flag] ->
       let fields = List.map object_field_of_yojson fields in
       let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag in
       Ptyp_object (fields, closed_flag)
     | _ -> failwith "Invalid JSON format for Ptyp_object")
  | `Assoc [("tag", `String "PtypClass"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; (`List core_types_json)] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Ptyp_class (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Ptyp_class")
  | `Assoc [("tag", `String "PtypAlias"); ("contents", jsons)] ->
    (match jsons with
     | `List [core_type_json; `String value] ->
       let core_type = core_type_of_yojson core_type_json in
       Ptyp_alias (core_type, value)
     | _ -> failwith "Invalid JSON format for Ptyp_alias")
  | `Assoc [("tag", `String "PtypVariant"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List row_fields); closed_flag; labels] ->
       let row_fields = List.map row_field_of_yojson row_fields in
       let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag in
       let labels_d = match labels with
          | `Null -> None
          | `List labels_list -> Some (List.map Asttypes_deserializer.label_of_yojson labels_list)
          | _ -> failwith "Invalid JSON format for labels of Ptyp_variant" in
       Ptyp_variant (row_fields, closed_flag, labels_d)
     | _ -> failwith "Invalid JSON format for Ptyp_variant")
  | `Assoc [("tag", `String "PtypPoly"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List strings); core_type_json] ->
       let stringss = List.map (Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x)) strings in
       let core_type = core_type_of_yojson core_type_json in
       Ptyp_poly (stringss, core_type)
     | _ -> failwith "Invalid JSON format for Ptyp_poly")
  | `Assoc [("tag", `String "PtypPackage"); ("contents", jsons)] -> package_type_of_yojson jsons
  | `Assoc [("tag", `String "PtypExtension"); ("contents", jsons)] -> extension_of_yojson jsons
  | _ -> failwith "Invalid JSON format for core_type_desc"

