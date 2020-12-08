type t = R of Refname.t | O of Oid.t

let pp ppf = function
  | R rname -> Format.fprintf ppf "ref:%a" Refname.pp rname
  | O oid -> Oid.pp ppf oid

let of_string s =
  match Oid.of_hex s with
  | oid -> O oid
  | exception _ -> (
      match Scanf.sscanf s "ref:%s" (fun s -> s) with
      | rname -> R (Refname.create rname)
      | exception _ ->
          let msg = Printf.sprintf "Ref.of_string: cannot parse %s" s in
          failwith msg )
