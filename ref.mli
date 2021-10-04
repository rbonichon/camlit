type t = R of Refname.t | O of Oid.t

val pp : Format.formatter -> t -> unit

val of_string : string -> t
(** [of_string s] takes a string value [s] and "does the right thing", that is:
 - if the string is a valid hex identifiier, it defines an oid;
 - other it assumes it is a ref 
*)
