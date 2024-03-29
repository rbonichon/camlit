let create_directory ?(warn = false) directory_name =
  if warn && Sys.file_exists directory_name then
    Format.eprintf "A directory or file named %s already exists@."
      directory_name;
  Unix.mkdir directory_name 0o700

let init () =
  create_directory File.default_directory;
  create_directory File.objects_directory;
  Data.set_head (Ref.R (Refname.create @@ File._head "master"))

let is_ignored =
  let rexp = Str.regexp_string Filename.dir_sep in
  fun path -> List.mem ".camlit" (Str.split rexp path)

let rec write_tree ~directory =
  let filenames =
    Sys.readdir directory |> Array.map (Filename.concat directory)
  in
  let entries =
    Array.fold_left
      (fun entries path ->
        if not @@ is_ignored path then
          let obj =
            let open Objects in
            if Sys.is_directory path then
              let oid = write_tree ~directory:path in
              tree (oid, path)
            else
              (* it is a file *)
              (* Format.printf "%s " path; *)
              let oid = File.read path |> Data.hash_string in
              blob (oid, path)
          in
          obj :: entries
        else entries)
      [] filenames
  in
  let ppf = Format.str_formatter in
  Format.pp_open_vbox ppf 0;
  List.iter
    (function
      | Objects.Blob (oid, path) ->
          Format.fprintf ppf "blob %a %s@," Oid.pp oid path
      | Objects.Tree (oid, path) ->
          Format.fprintf ppf "tree %a %s@," Oid.pp oid path
      | Objects.Commit _ -> assert false)
    entries;
  Format.pp_close_box ppf ();
  let o = Objects.tree (Format.flush_str_formatter ()) in
  Data.hash_object o

let _tree_entries oid =
  let contents = Data.get_tree oid in
  Format.printf "contents: %s@." contents;
  String.split_on_char '\n' (String.trim contents)
  |> List.map (fun line ->
         Scanf.sscanf line "%s %[a-f0-9] %s" (fun typ oid name ->
             match typ with
             | "blob" -> Objects.blob (oid, name)
             | "tree" -> Objects.tree (oid, name)
             | typ ->
                 let msg =
                   Format.asprintf "unexpected type for tree entry (%s) oid %s"
                     typ oid
                 in
                 failwith msg))

let get_tree oid path =
  let h = Hashtbl.create 7 in
  let rec loop oid path =
    let entries = _tree_entries oid in
    List.iter
      (function
        | Objects.Blob (oid, name) ->
            Hashtbl.add h (Filename.concat path name) (Oid.of_hex oid)
        | Objects.Tree (oid, name) ->
            loop (Oid.of_hex oid) (Filename.concat path name)
        | Objects.Commit _ -> ())
      entries
  in
  loop oid path;
  h

let empty_directory path =
  if Sys.is_directory path then (
    let w = File.walk path in
    let delete_with rm name = if not @@ is_ignored name then rm name in
    List.iter (delete_with Sys.remove) w.files;
    List.iter
      (delete_with (fun dirname -> try Unix.rmdir dirname with _ -> ()))
      w.directories )

let read_tree oid =
  let path = "." in
  empty_directory path;
  let h = get_tree oid path in
  Hashtbl.iter
    (fun name oid ->
      let oc = open_out_bin name in
      Printf.fprintf oc "%s" (Data.get_object oid |> Objects.contents);
      flush oc;
      close_out oc)
    h

let commit ~message =
  print_endline message;
  let dir_oid = write_tree ~directory:"." in
  let parent = Data.get_head () in
  let oid = Data.hash_object (Objects.commit dir_oid ?parent message) in
  Data.set_head (Ref.O oid);
  oid

let get_commit oid = Data.get_commit oid

let tag_oid name oid =
  let tagfile = File._tag (Filename.concat "tags" name) in
  Data.update_ref (Refname.create tagfile) (Ref.O oid)

let get_oid name =
  match Data.find_ref (Refname.create name) with
  | Some oid -> oid
  | None -> Oid.of_hex name

let create_branch name oid =
  Data.update_ref (Refname.create @@ File._head name) (Ref.O oid);
  Format.printf "Branch %s created at %a@." name Oid.pp oid

let is_branch name =
  let headfile = File._head name in
  Sys.file_exists headfile
  &&
  match Ref.of_string @@ File.read headfile with
  | exception _ -> false
  | R _ | O _ -> true

let checkout name =
  let oid = get_oid name in
  let commit = get_commit oid in
  read_tree commit.oid;
  let _ref =
    if is_branch name then Ref.R (Refname.create @@ File._head name)
    else Ref.O oid
  in
  Data.set_head _ref

let branch_basename =
  let start = "refs/heads/" in
  fun (Refname.Refname refname) ->
    if not @@ File.startswith start refname then None
    else
      let len = String.length start in
      Some (String.sub refname len (String.length refname - len))

let get_branch_name () =
  match Data.get_direct_ref Data.head with
  | None -> None
  | Some (Ref.O _) -> None (* this is an oid not a name *)
  | Some (Ref.R _ref) -> branch_basename _ref

let branch_names () =
  List.map
    (fun (Refname.Refname branch_name, _) -> branch_name)
    (Data.get_refs ~prefix:"refs/heads/" ())
