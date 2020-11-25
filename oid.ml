type t = Oid of string

type oid = t

module Set = Set.Make (struct
  type t = oid

  let compare (Oid o1) (Oid o2) = String.compare o1 o2
end)

let of_string s = Oid (Digest.string s)

let of_file file = Oid (Digest.file file)

let to_string (Oid oid) = Digest.to_hex oid

let of_hex hex = Oid (Digest.from_hex hex)

let pp ppf t = Format.pp_print_string ppf (to_string t)
