type refname = Refname of string

let refname n = Refname n

type oid = Oid of Hash.t

let oid h = Oid h

type _ t = Symbolic : refname -> refname t | Direct : oid -> oid t

let symbolic s = Symbolic s

let direct o = Direct o
