(** High level command line units *)

val init : unit -> unit
(** Initialize the current working directory as a mugit instance, that is,
 ** create a .mugit *)

val cat_file : Oid.t -> unit

val hash_file : string -> unit

val checkout : string -> unit

val reset : Oid.t -> unit

val log : Oid.t -> unit

val status : unit -> unit

val show : Oid.t -> unit
