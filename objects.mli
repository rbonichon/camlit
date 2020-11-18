module Type : Sigs.STRINGIFIABLE

type t = private { typ : Type.t option; contents : string }

include Sigs.STRINGIFIABLE with type t := t
