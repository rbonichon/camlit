type hash = string

let hash_string s =
  Format.sprintf "%016x" (Hashtbl.hash s)
