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
