type oid = Hash.t

type oid_set = Hash.Set.t

type refname = string 

type path = string 

val set_head : oid -> unit
(** [set_head] *)

val get_head : unit -> oid option
(** [get_head] *)

val get_ref : ?under:path -> refname -> oid option
(** [get_ref ~under refname] retrieves the hash value contained in reffile
   [refname] under directory [under], if specified.
   
   [under] defaults to [File.default_directory]
 *)

val find_ref : refname -> oid option
(** [find_ref refname] is like [get_ref] but searches for the reffile under 
    the following directory, in that order 
    - [File.default_directory]
    - [File.tags_directory]
    - [File.refs_directory]
    - [File.heads_directory]
 *)

val update_ref : refname -> oid -> unit
(** [update_ref] *)

val hash_string : string -> oid
(** [hash_string] *)

val get_object : oid -> string Objects.t
(** [get_object] *)

val hash_object : string Objects.t -> oid
(** [hash_object] *)

val get_commit : oid -> Objects.commit
(** [get_commit] *)

val get_tree : oid -> string
(** [get_tree] *)

val get_blob : oid -> string
(** [get_blob] *)


(** [get_refs] *)
val get_refs: unit -> (refname * oid) list


(** [predecessors oids] computes the transitive closure of predecessors for
   these oids *)
val predecessors: oid list -> oid list ;;
