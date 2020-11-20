type t

include Sigs.STRINGIFIABLE with type t := t

val of_file : string -> t

val pp : Format.formatter -> t -> unit
