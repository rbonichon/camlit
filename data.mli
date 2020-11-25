val head : Reference.refname Reference.t

val set_head : Reference.oid Reference.t -> unit
(** [set_head] *)

val get_head : unit -> Reference.oid Reference.t option
(** [get_head] *)

val get_ref :
  ?under:Types.path -> Reference.refname -> Reference.oid Reference.t option
(** [get_ref ~under refname] retrieves the hash value contained in reffile
   [refname] under directory [under], if specified.
   
   [under] defaults to [File.default_directory]
 *)

val find_ref : Reference.refname -> Reference.oid option
(** [find_ref refname] is like [get_ref] but searches for the reffile under 
    the following directory, in that order 
    - [File.default_directory]
    - [File.tags_directory]
    - [File.refs_directory]
    - [File.heads_directory]
 *)

val update_ref :
  Reference.refname Reference.t -> Reference.oid Reference.t -> unit
(** [update_ref] *)

val hash_string : string -> Reference.oid
(** [hash_string] *)

val get_object : Reference.oid -> string Objects.t
(** [get_object] *)

val hash_object : string Objects.t -> Reference.oid
(** [hash_object] *)

val get_commit : Reference.oid -> Objects.commit
(** [get_commit] *)

val get_tree : Reference.oid -> string
(** [get_tree] *)

val get_blob : Reference.oid -> string
(** [get_blob] *)

val get_refs : unit -> (Reference.refname * Reference.oid Reference.t) list
(** [get_refs] *)

val predecessors : Reference.oid list -> Reference.oid Reference.t list
(** [predecessors oids] computes the transitive closure of predecessors for
   these oids *)
