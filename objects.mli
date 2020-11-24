(** Git objects manipulation *)

(** {1 Objects }*)

type contents = string

type commit = { oid : Hash.t; parent : Hash.t option; message : string }

type _ t = Tree : 'a -> 'a t | Blob : 'a -> 'a t | Commit : commit -> 'a t

(* type 'a t = Tree of 'a | Blob of 'a | Commit of commit *)

include Sigs.STRINGIFIABLE with type t := contents t

val tree : 'a -> 'a t

val blob : 'a -> 'a t

val commit : ?parent:Hash.t -> Hash.t -> string -> 'a t

val contents : contents t -> string

val hash : contents t -> Hash.t
