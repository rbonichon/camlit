val write_tree : directory:string -> Hash.t
(** [write_tree directory] *)

val read_tree : Reference.oid -> unit
(** [read_tree] *)

val commit : message:string -> Reference.oid

val checkout : Reference.oid -> unit

val tag_oid : string -> Reference.oid -> unit

val get_oid : Reference.refname -> Reference.oid

val create_branch : Types.branch_name -> Reference.oid -> unit
