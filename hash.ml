type t = string

let hash_string s = Format.sprintf "%016x" (Hashtbl.hash s)

let pp = Format.pp_print_string
