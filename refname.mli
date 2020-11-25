type t = private Refname of string

val create : string -> t

val pp : Format.formatter -> t -> unit
