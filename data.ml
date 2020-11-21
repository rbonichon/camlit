let hash_object obj =
  let oid = Objects.hash obj in
  let oc = open_out_bin (File._object oid) in
  Printf.fprintf oc "%s" (Objects.to_string obj);
  flush oc;
  close_out oc;
  oid

let hash_string s = Objects.blob s |> hash_object

let check_object_type objstr o =
  match objstr with
  | None -> ()
  | Some ostr ->
      let otyp =
        let open Objects in
        match o with Tree _ -> "tree" | Blob _ -> "blob" | Commit _ -> "commit"
      in
      if ostr <> otyp then
        let msg =
          Printf.sprintf "Object type mismatch. Expected %s, got %s" ostr otyp
        in
        failwith msg

let get_object oid = File.(read (_object oid)) |> Objects.of_string

let get_commit oid =
  match get_object oid with 
  | Objects.Commit v -> v
  | _ ->
     let msg = Format.asprintf "%a does not correspond to a commit" Hash.pp oid in
     failwith msg 

let get_tree oid =
  match get_object oid with
  | Objects.Tree contents -> contents
  | _ -> assert false

let get_blob oid =
  match get_object oid with
  | Objects.Blob contents -> contents
  |  _ -> assert false


let set_head oid =
  let oc = open_out_bin File.head in
  let ppf = Format.formatter_of_out_channel oc in
  Hash.pp ppf oid;
  Format.pp_print_flush ppf ();
  close_out oc

let get_head () =
  if Sys.file_exists File.head then
    File.read File.head |> String.trim |> Hash.of_hex |> Option.some
  else None 

