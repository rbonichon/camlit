type t

include Sigs.STRINGIFIABLE with type t := t

module Set: Set.S with type elt = t

val of_file : string -> t

val of_hex : string -> t

val pp : Format.formatter -> t -> unit
