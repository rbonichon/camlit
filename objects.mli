
module Type : sig
  type t = Blob 
  include Sigs.STRINGIFIABLE with type t := t
end

type t = private { typ : Type.t option; contents : string }

include Sigs.STRINGIFIABLE with type t := t

val create : ?typ:Type.t -> string -> t

val hash : t -> Hash.hash 
