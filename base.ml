let is_ignored =
  let rexp = Str.regexp_string Filename.dir_sep in
  fun path -> List.mem ".camlit" (Str.split rexp path)

let rec write_tree ~directory =
  let filenames =
    Sys.readdir directory |> Array.map (Filename.concat directory)
  in
  let entries = 
    Array.fold_left
      (fun entries path  ->
        if not @@ is_ignored path then
          let open Objects.Type in 
          let oid, typ = 
          if Sys.is_directory path then
            let oid = write_tree ~directory:path in
            oid, Tree
          else (
          (* it is a file *)
            (* Format.printf "%s " path; *)
            let file_contents = File.read path in
            let oid = Cmds.hash_string file_contents in
            oid, Blob)
          in (path, oid, typ) :: entries
       else entries
      )
      []
      filenames
  in
  let ppf = Format.str_formatter in
  Format.pp_open_vbox ppf 0;
  List.iter
    (fun (path, oid, typ) ->
      Format.fprintf ppf "%s %a %s@,"
        (Objects.Type.to_string typ) Hash.pp oid path) entries;
  Format.pp_close_box ppf ();
  let o = Objects.tree ~contents:(Format.flush_str_formatter ()) in
  Cmds.hash_object o
;;


let _tree_entries oid =
  Cmds.get_object oid 
  
let read_tree _oid = assert false
  
  

  
