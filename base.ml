let is_ignored =
  let rexp = Str.regexp_string Filename.dir_sep in
  fun path ->
  List.mem ".camlit" (Str.split rexp path)


let rec write_tree ?(directory=".") () =
  let filenames = Sys.readdir directory |> Array.map (Filename.concat directory) in
  Array.iter
    (fun path ->
      if not @@ is_ignored path then
        (if Sys.is_directory path 
        then write_tree ~directory:path ()
        else Format.printf "%s@." path))
    filenames
