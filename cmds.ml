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

let hash_file file =
  File.read file |> Data.hash_string |> Format.printf "%a@." Hash.pp

let cat_file oid =
  Data.get_object oid |> Objects.contents |> Format.printf "%s@."
