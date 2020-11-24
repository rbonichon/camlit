

(** [set_head] *)
val set_head: Hash.t -> unit ;;

(** [get_head] *)
val get_head: unit -> Hash.t option ;;



(** [get_ref ~under refname] retrieves the hash value contained in reffile
   [refname] under directory [under], if specified.
   
   [under] defaults to [File.default_directory]
 *)
val get_ref: ?under:string -> string -> Hash.t option ;;


(** [find_ref refname] is like [get_ref] but searches for the reffile under 
    the following directory, in that order 
    - [File.default_directory]
    - [File.tags_directory]
    - [File.refs_directory]
    - [File.heads_directory]
 *)
val find_ref: string -> Hash.t option ;;

(** [update_ref] *)
val update_ref: string -> Hash.t -> unit ;;

(** [hash_string] *)
val hash_string: string -> Hash.t ;;


(** [get_object] *)
val get_object: Hash.t -> string Objects.t ;;


(** [hash_object] *)
val hash_object: string Objects.t -> Hash.t ;;

(** [get_commit] *)
val get_commit: Hash.t -> Objects.commit ;;


(** [get_tree] *)
val get_tree: Hash.t -> string ;;

(** [get_blob] *)
val get_blob: Hash.t -> string ;;
