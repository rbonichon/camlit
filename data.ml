let hash_object obj =
  let oid = Objects.hash obj in
  let oc = open_out_bin (File._object oid) in
  Printf.fprintf oc "%s" (Objects.to_string obj);
  flush oc;
  close_out oc;
  oid

let hash_string s = Objects.blob s |> hash_object

let get_object oid = File.(read (_object oid)) |> Objects.of_string

let get_commit oid =
  match get_object oid with
  | Objects.Commit v -> v
  | _ ->
      let msg =
        Format.asprintf "%a does not correspond to a commit" Hash.pp oid
      in
      failwith msg

let get_tree oid =
  match get_object oid with
  | Objects.Tree contents -> contents
  | _ -> assert false

let get_blob oid =
  match get_object oid with
  | Objects.Blob contents -> contents
  | _ -> assert false

let update_ref refname oid =
  let file = File._ref refname in
  File.makedirs file;
  let oc = open_out_bin file in
  let ppf = Format.formatter_of_out_channel oc in
  Hash.pp ppf oid;
  Format.pp_print_flush ppf ();
  close_out oc

let get_hash_from_file filename =
  if Sys.file_exists filename then
    File.read filename |> String.trim |> Hash.of_hex |> Option.some
  else None

let get_ref ?under refname =
  let filename =
    match under with
    | None -> File._ref refname
    | Some dir -> Filename.concat dir refname
  in
  get_hash_from_file filename

let find_ref refname =
  let prefixes =
    [
      File.default_directory;
      File.refs_directory;
      File.tags_directory;
      File.heads_directory;
    ]
  in
  let rec find = function
    | [] -> None
    | dir :: dirs -> (
        let filename = Filename.concat dir refname in
        match get_hash_from_file filename with
        | None -> find dirs
        | some_hash -> some_hash )
  in
  find prefixes

let set_head = update_ref "HEAD"

let get_head () = get_ref "HEAD"
