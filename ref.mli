type t = R of Refname.t | O of Oid.t

val pp : Format.formatter -> t -> unit

val of_string : string -> t
