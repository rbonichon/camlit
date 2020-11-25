type t = Refname of string

let create s = Refname s

let pp ppf (Refname name) = Format.pp_print_string ppf name
