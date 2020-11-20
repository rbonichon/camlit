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
          Format.fprintf ppf "blob %a %s@," Hash.pp oid path
      | Objects.Tree (oid, path) ->
         Format.fprintf ppf "tree %a %s@," Hash.pp oid path
      | Objects.Commit _ -> assert false)
    entries;
  Format.pp_close_box ppf ();
  let o = Objects.tree (Format.flush_str_formatter ()) in
  Data.hash_object o

let _tree_entries oid =
  let contents = Data.get_tree oid in
  String.split_on_char '\n' contents
  |> List.map (fun line ->
         Scanf.sscanf line "%s %[a-f0-9] %s" (fun typ oid name ->
             match typ with
             | "blob" -> Objects.blob (oid, name)
             | "tree" -> Objects.tree (oid, name)
             | _ -> assert false))

let get_tree oid path =
  let h = Hashtbl.create 7 in
  let rec loop oid path =
    let entries = _tree_entries oid in
    List.iter
      (function
        | Objects.Blob (oid, name) ->
            Hashtbl.add h (Filename.concat path name) oid
        | Objects.Tree (oid, name) ->
           loop oid (Filename.concat path name)
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
  let oid = Data.hash_object (Objects.commit dir_oid message) in
  Data.set_head oid;
  oid 

