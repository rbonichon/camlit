let default_directory = ".camlit"

let objects_directory = Filename.concat default_directory "objects"

let create_directory ?(warn = false) directory_name =
  if warn && Sys.file_exists directory_name then
    Format.eprintf "A directory or file named %s already exists@."
      directory_name;
  Unix.mkdir directory_name 0o700

let init () =
  Format.printf "Initializing directory %s@." default_directory;
  create_directory default_directory;
  create_directory objects_directory

let hash_string s =
  Format.sprintf "%016x" (Hashtbl.hash s)

let hash_object file =
  File.read file
  |> hash_string
  |> Format.printf "%s@." hex
  
