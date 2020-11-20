(** Git objects manipulation *)

(** {1 Objects }*)

type contents = string

type 'a t = Tree of 'a | Blob of 'a | Commit of { oid : Hash.t; message: string; }

include Sigs.STRINGIFIABLE with type t := contents t

val tree : 'a -> 'a t

val blob : 'a -> 'a t

val commit : Hash.t -> string -> 'a t

val contents : contents t -> string

val hash : contents t -> Hash.t
