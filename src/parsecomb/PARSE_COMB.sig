(** Simple parser combinator library that keeps track of position information. *)
signature PARSE_COMB = sig
  type token
  datatype ('a,'b) either = OK of 'a | NO of 'b
  type locerr = Region.loc * (unit -> string)
  type 'a p = (token*Region.reg)list -> ('a * Region.reg * (token*Region.reg)list, locerr) either
    
  val >>> : 'a p * 'b p -> ('a*'b)p
  val ->> : unit p * 'b p -> 'b p
  val >>- : 'a p * unit p -> 'a p
  val ??  : 'a p * 'b p -> ('a*'b -> 'a) -> 'a p
  val ??? : 'a p * 'b p -> ('a*'b*Region.reg -> 'a) -> 'a p
  val ||  : 'a p * 'a p -> 'a p
  val oo  : 'a p * ('a -> 'b) -> 'b p
  val ign : 'a p -> unit p
  val eat : token -> unit p
  val oor : 'a p * ('a*Region.reg -> 'b) -> 'b p
end

(**
[token] type of tokens.

['a p] type of parsers that parse values of type 'a.

a >>> b
    Sequence parsers a and b

a ->> b
    Sequence parsers a and b, discard result of a

a >>- b
    Sequence parsers a and b, discard result of b

(a ?? b) f
    parse a and maybe continue with b, if both succeeds, combine with f

(a ??? b) f
    same as ??, but giving region information to f

a || b
    alternatives

p oo f
    fmap f p

ign p
    discard the result of a parser p

eat t ts
    "eat" one token t from list ts

p oor f
    fmap f p, giving region info to f
*)
