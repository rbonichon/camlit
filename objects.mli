
(** Git objects manipulation *)

(** {1 Object types}*)
module Type : sig
  type t = Blob | Tree

  include Sigs.STRINGIFIABLE with type t := t
end

(** {1 Objects }*)

(** An object is an optional type with bytes representing its contents *)
type t =
  private
    {
      typ : Type.t option;
      contents : string;
    }

include Sigs.STRINGIFIABLE with type t := t

val create : ?typ:Type.t -> string -> t

val tree: contents:string -> t

val blob: contents:string -> t

val hash : t -> Hash.t
