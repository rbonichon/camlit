open File

let create_directory ?(warn = false) directory_name =
  if warn && Sys.file_exists directory_name then
    Format.eprintf "A directory or file named %s already exists@."
      directory_name;
  Unix.mkdir directory_name 0o700

let init () =
  Format.printf "Initializing directory %s@." default_directory;
  create_directory default_directory;
  create_directory objects_directory

let hash_object obj =
  let oid = Objects.hash obj in
  let oc = open_out_bin (Filename.concat objects_directory (oid :> string)) in
  Printf.fprintf oc "%s" (Objects.to_string obj);
  flush oc;
  close_out oc;
  oid
  
let hash_string s =
  Objects.create s |> hash_object

let get_object file =
  File.read file |> hash_string |> Format.printf "%a@." Hash.pp

let check_object_type obj = function
  | None -> ()
  | Some t as ty ->
      let otyp = obj.Objects.typ in
      if ty <> otyp then
        let msg =
          Printf.sprintf "Object type mismatch. Expected %s, got %s"
            (Objects.Type.to_string t)
            ( match otyp with
            | None -> "none"
            | Some t -> Objects.Type.to_string t )
        in
        failwith msg

let cat_file ?expected oid =
  let o = File.read (_object oid) |> Objects.of_string in
  check_object_type o expected;
  Format.printf "%s@." o.contents
