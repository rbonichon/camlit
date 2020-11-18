let default_directory = ".camlit"

let create_directory ?(warn = false) directory_name =
  if warn && Sys.file_exists directory_name then
    Format.eprintf "A directory or file named %s already exists@."
      directory_name;
  Unix.mkdir directory_name 0o700

let init () =
  Format.printf "Initializing directory %s@." default_directory;
  create_directory default_directory
