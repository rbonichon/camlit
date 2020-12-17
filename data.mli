type path = string

val set_head : Ref.t -> unit
(** [set_head] *)

val get_head : unit -> Oid.t option
(** [get_head] *)

val get_ref : Ref.t -> Oid.t option
(** [get_ref ~under refname] retrieves the hash value contained in reffile
   [refname] under directory  *)

val find_ref : Refname.t -> Oid.t option
(** [find_ref refname] is like [get_ref] but searches for the reffile under 
    the following directory, in that order 
    - [File.default_directory]
    - [File.tags_directory]
    - [File.refs_directory]
    - [File.heads_directory]
 *)

val update_ref : Refname.t -> Ref.t -> unit
(** [update_ref] *)

val hash_string : string -> Oid.t
(** [hash_string] *)

val get_object : Oid.t -> string Objects.t
(** [get_object] *)

val hash_object : string Objects.t -> Oid.t
(** [hash_object] *)

val get_commit : Oid.t -> Objects.commit
(** [get_commit] *)

val get_tree : Oid.t -> string
(** [get_tree] *)

val get_blob : Oid.t -> string
(** [get_blob] *)

val get_refs : ?prefix:string -> unit -> (Refname.t * Oid.t) list
(** [get_refs] 
    Strips prefix in [Refname.t] 
*)

val predecessors : Oid.t list -> Oid.t list
(** [predecessors oids] computes the transitive closure of predecessors for
   these oids *)

val get_direct_ref : Ref.t -> Ref.t option
(** [get_direct_ref] *)

val head : Ref.t
