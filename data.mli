type path = string 

val set_head : Types.oid -> unit
(** [set_head] *)

val get_head : unit -> Types.oid option
(** [get_head] *)

val get_ref : ?under:path -> Types.refname -> Types.oid option
(** [get_ref ~under refname] retrieves the hash value contained in reffile
   [refname] under directory [under], if specified.
   
   [under] defaults to [File.default_directory]
 *)

val find_ref : Types.refname -> Types.oid option
(** [find_ref refname] is like [get_ref] but searches for the reffile under 
    the following directory, in that order 
    - [File.default_directory]
    - [File.tags_directory]
    - [File.refs_directory]
    - [File.heads_directory]
 *)

val update_ref : Types.refname -> Types.oid -> unit
(** [update_ref] *)

val hash_string : string -> Types.oid
(** [hash_string] *)

val get_object : Types.oid -> string Objects.t
(** [get_object] *)

val hash_object : string Objects.t -> Types.oid
(** [hash_object] *)

val get_commit : Types.oid -> Objects.commit
(** [get_commit] *)

val get_tree : Types.oid -> string
(** [get_tree] *)

val get_blob : Types.oid -> string
(** [get_blob] *)

(** [get_refs] *)
val get_refs: unit -> (Types.refname * Types.oid) list

(** [predecessors oids] computes the transitive closure of predecessors for
   these oids *)
val predecessors: Types.oid list -> Types.oid list ;;
