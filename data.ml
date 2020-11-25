let hash_object obj =
  let oid = Objects.hash obj in
  let oc = open_out_bin (File._object oid) in
  Printf.fprintf oc "%s" (Objects.to_string obj);
  flush oc;
  close_out oc;
  Reference.oid oid

let hash_string s = Objects.blob s |> hash_object

let get_object (Reference.Oid oid) =
  File.(read (_object oid)) |> Objects.of_string

let get_commit (Reference.Oid h as oid) =
  match get_object oid with
  | Objects.Commit v -> v
  | _ ->
      let msg =
        Format.asprintf "%a does not correspond to a commit" Hash.pp h
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

let update_ref (Reference.Symbolic (Refname refname))
    (Reference.Direct (Oid oid)) =
  let file = File._ref refname in
  File.makedirs file;
  let oc = open_out_bin file in
  let ppf = Format.formatter_of_out_channel oc in
  Hash.pp ppf (oid :> Hash.t);
  Format.pp_print_flush ppf ();
  close_out oc

let get_hash_from_file filename =
  if Sys.file_exists filename then
    File.read filename |> String.trim |> Hash.of_hex |> Reference.oid
    |> Option.some
  else None

type file_contents =
  | Contents : 'a Reference.t -> file_contents
  | Nothing : file_contents

let read_ref_in_file filename =
  if Sys.file_exists filename then
    let contents = File.read filename |> String.trim in
    match Scanf.sscanf contents "ref: %s" (fun s -> s) with
    | exception _ ->
        Contents (Reference.direct @@ Reference.oid (Hash.of_hex contents))
    | s -> Contents (Reference.symbolic @@ Reference.refname s)
  else Nothing

let rec resolve_ref : type a. a Reference.t -> Reference.oid Reference.t option
    = function
  | Direct _ as oid -> Some oid
  | Symbolic (Refname refname) -> (
      match read_ref_in_file refname with
      | Nothing -> None
      | Contents reference -> resolve_ref reference )

let get_ref ?under (Reference.Refname refname) =
  let filename =
    match under with
    | None -> File._ref (refname :> string)
    | Some dir -> Filename.concat dir (refname :> string)
  in
  resolve_ref (Reference.symbolic @@ Reference.refname filename)

let find_ref (Reference.Refname refname) =
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
        let filename = Filename.concat dir (refname :> string) in
        match get_hash_from_file filename with
        | None -> find dirs
        | some_hash -> some_hash )
  in
  find prefixes

let head = Reference.symbolic (Reference.refname "HEAD")

let set_head = update_ref head

let get_head : unit -> Reference.oid Reference.t option =
 fun () -> get_ref (Reference.refname "HEAD")

let get_refs () =
  let walk = File.walk File.refs_directory in
  List.map
    (fun filename ->
      ( Reference.refname filename,
        Option.get @@ get_ref ~under:"." (Reference.refname filename) ))
    walk.files

let predecessors oids =
  let rec loop visited set = function
    | [] -> set
    | (Reference.Oid od as oid) :: oids ->
        if Hash.Set.mem od visited then loop visited set oids
        else
          let set = Reference.direct oid :: set in
          let oids =
            let commit = get_commit oid in
            match commit.parent with
            | None -> oids
            | Some oid -> Reference.oid oid :: oids
          in
          loop (Hash.Set.add od visited) set oids
  in
  loop Hash.Set.empty [] oids
