type t = string

let default_directory = ".camlit"

let objects_directory = Filename.concat default_directory "objects"

(* object is reserved in OCaml *)
let _object = Filename.concat objects_directory

let read file =
  let ic = open_in file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let makedirs split_path =
  let rec loop path = function
    | [] | [ _ ] -> ()
    | pa :: pas ->
        let dir = Filename.concat path pa in
        if not @@ Sys.file_exists dir then Unix.mkdir dir 0o700;
        loop dir pas
  in
  loop "." split_path

let makedirs path =
  makedirs (Str.split (Str.regexp_string Filename.dir_sep) path)
