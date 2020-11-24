type t = string

module Set = Set.Make(String)

let of_string s = Digest.string s

let of_file = Digest.file

let to_string = Digest.to_hex

let of_hex = Digest.from_hex

let pp ppf t = Format.pp_print_string ppf (to_string t)
