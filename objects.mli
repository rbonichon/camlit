
(** Git objects manipulation *)


(** {1 Objects }*)


type contents = string 

(** An object is an optional type with bytes representing its contents *)
type 'a t = 
  | Tree of 'a 
  | Blob of 'a 


include Sigs.STRINGIFIABLE with type t := contents t

val tree: 'a -> 'a t

val blob: 'a -> 'a t

val contents: contents t -> string

val hash : contents t -> Hash.t
