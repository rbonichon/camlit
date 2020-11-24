type t = string

let default_directory = ".camlit"

let objects_directory = Filename.concat default_directory "objects"

let refs_directory = Filename.concat default_directory "refs"

let tags_directory = Filename.concat refs_directory "tags"

let heads_directory = Filename.concat refs_directory "heads"

let _ref = Filename.concat default_directory

let _tag = Filename.concat "refs"

(* object is reserved in OCaml *)
let _object oid = Filename.concat objects_directory (Hash.to_string oid)

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

type walk = { directories : string list; files : string list; root : string }

let walk path =
  let root = path in
  let rec loop files directories = function
    | [] -> { root; files; directories }
    | dirname :: dirs ->
        let a = Sys.readdir dirname in
        let files, dirs =
          Array.fold_left
            (fun (files, dirs) elt ->
              let elt = Filename.concat dirname elt in
              if Sys.is_directory elt then (files, elt :: dirs)
              else (elt :: files, dirs))
            (files, dirs) a
        in
        loop files (dirname :: directories) dirs
  in
  loop [] [] [ path ]
