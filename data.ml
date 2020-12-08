type path = string

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
        Format.asprintf "%a does not correspond to a commit" Oid.pp oid
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

let get_hash_from_file filename =
  if Sys.file_exists filename then
    File.read filename |> String.trim |> Oid.of_hex |> Option.some
  else None

open Refname

let get_ref_from_file filename =
  if Sys.file_exists filename then
    match Ref.of_string (File.read filename |> String.trim) with
    | value -> Some value
    | exception _ -> None
  else None

let get_direct_ref = function
  | Ref.R (Refname r) -> get_ref_from_file (File._ref r)
  | _ref -> Some _ref

let rec get_ref_loop _ref =
  match get_direct_ref _ref with
  | None -> (_ref, None)
  | Some (O oid) -> (_ref, Some oid)
  | Some r -> get_ref_loop r

let get_ref _ref = get_ref_loop _ref |> snd

let update_ref refname oid =
  let open Ref in
  let _ref, _ = get_ref_loop (R refname) in
  match _ref with
  | R (Refname refname) ->
      let file = File._ref refname in
      File.makedirs file;
      let oc = open_out_bin file in
      let ppf = Format.formatter_of_out_channel oc in
      Oid.pp ppf oid;
      Format.pp_print_flush ppf ();
      close_out oc
  | O _ -> failwith "update_ref: expected a reference name not oid"

let find_ref (Refname refname) =
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

let head = Refname.create "HEAD"

let set_head = update_ref head

let get_head () = get_ref (R head)

let get_refs () =
  let walk = File.walk File.refs_directory in
  List.map
    (fun filename ->
      let _ref = Refname.create filename in
      (_ref, Option.get @@ get_ref (R _ref)))
    walk.files

let predecessors oids =
  let rec loop visited set = function
    | [] -> set
    | oid :: oids ->
        if Oid.Set.mem oid visited then loop visited set oids
        else
          let set = oid :: set in
          let oids =
            let commit = get_commit oid in
            match commit.parent with None -> oids | Some oid -> oid :: oids
          in
          loop (Oid.Set.add oid visited) set oids
  in
  loop Oid.Set.empty [] oids
