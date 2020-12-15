val write_tree : directory:string -> Oid.t
(** [write_tree directory] *)

val read_tree : Oid.t -> unit
(** [read_tree] *)

val commit : message:string -> Oid.t

val checkout : string -> unit

val tag_oid : string -> Oid.t -> unit

val get_oid : string -> Oid.t

val create_branch : Types.branch_name -> Oid.t -> unit

val is_branch : string -> bool

val init : unit -> unit
(** [init ()] setup the default structure of a git repository *)

val get_branch_name : unit -> string option
(** [get_branch_name ()] *)
