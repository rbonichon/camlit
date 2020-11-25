type refname = private Refname of string

val refname : string -> refname

type oid = private Oid of Hash.t

val oid : Hash.t -> oid

type _ t = private Symbolic : refname -> refname t | Direct : oid -> oid t

val symbolic : refname -> refname t

val direct : oid -> oid t
(** [direct] *)
