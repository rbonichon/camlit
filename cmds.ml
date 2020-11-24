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

(* Add n whitespaces after each new lines to string s*)
let wrap n ppf s =
  let len = String.length s in
  let blanks = String.make n ' ' in
  let rec loop indent idx =
    if idx < len then (
      if indent then Format.fprintf ppf "%s" blanks;
      let c = s.[idx] in
      Format.pp_print_char ppf c;
      loop (c = '\n') (idx + 1) )
  in
  loop true 0

let log oid  =
  let rec loop = function
    | None -> Format.print_flush ()
    | Some oid ->
        let cmt = Data.get_commit oid in
        Format.printf "commit %a@\n%a@\n@." Hash.pp oid (wrap 4) cmt.message;
        loop cmt.parent
  in
  loop (Some oid)

let checkout = Base.checkout
