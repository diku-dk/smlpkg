signature FINMAP_EQ = sig
  type ('a,'b) t
  val empty    : ('a*'a -> bool) -> ('a,'b)t
  val empty_eq : unit -> (''a,'b)t
  val lookup   : ('a,'b)t -> 'a -> 'b option
  val add      : 'a*'b -> ('a,'b)t -> ('a,'b)t
  val fold     : ('b*'c -> 'c) -> 'c -> ('a,'b)t -> 'c
  val Fold     : (('a*'b)*'c -> 'c) -> 'c -> ('a,'b)t -> 'c
  val toString : ('a -> string) * ('b -> string) -> ('a,'b)t -> string

  val fromList : ('a*'a->bool) -> ('a*'b)list -> ('a,'b)t
  val fromList_eq : (''a*'b)list -> (''a,'b)t

  val keys     : ('a,'b)t -> 'a list

  val toList   : ('a,'b)t -> ('a*'b)list
end
