val write_tree : directory:string -> Hash.t
(** [write_tree directory] *)

val read_tree : Hash.t -> unit
(** [read_tree] *)

val commit : message:string -> Hash.t

val checkout : Hash.t -> unit

val tag : string -> unit

val tag_oid : Hash.t -> string -> unit

val get_oid : string -> Hash.t
