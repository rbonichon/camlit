let hash_object obj =
  let oid = Objects.hash obj in
  let oc = open_out_bin (File._object (Hash.to_string oid)) in
  Printf.fprintf oc "%s" (Objects.to_string obj);
  flush oc;
  close_out oc;
  oid

let hash_string s =
  Objects.blob s |> hash_object

let check_object_type objstr o =
  match objstr with
  | None -> ()
  | Some ostr -> 
  let otyp =
    let open Objects in
    match o with 
    | Tree _ -> "tree"
    | Blob _ -> "blob"
  in
  if ostr <> otyp then
    let msg =
      Printf.sprintf "Object type mismatch. Expected %s, got %s"
        ostr otyp
    in
    failwith msg

let get_object oid =
  File.(read (_object oid)) |> Objects.of_string

let get_tree oid =
  match get_object oid with
  | Objects.Tree contents -> contents
  | Objects.Blob _ -> assert false

let get_blob oid = 
  match get_object oid with
  | Objects.Blob contents -> contents
  | Objects.Tree _ -> assert false

  

