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


let update_ref refname oid =
  let file = File._ref refname in
  File.makedirs file;
  let oc = open_out_bin file in
  let ppf = Format.formatter_of_out_channel oc in
  Hash.pp ppf oid;
  Format.pp_print_flush ppf ();
  close_out oc

let get_ref refname =
  let file = File._ref refname in
  if Sys.file_exists file then 
    File.read file |> String.trim |> Hash.of_hex |> Option.some
  else None 

let set_head = update_ref "HEAD"

let get_head () = get_ref "HEAD"

