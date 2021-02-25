type reg
type debruijn
type 'a t
(* Phantom types are cool! *)

val create: Lambda_expr.t -> reg t

val parse: string -> reg t

val print: 'a t -> unit

val to_debruijn: reg t -> debruijn t

val aoe: reg t -> int -> reg t

val nor: reg t -> int -> reg t

val eta: reg t -> int -> reg t
