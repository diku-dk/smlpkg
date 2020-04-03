structure FinMapEq :> FINMAP_EQ = struct
  type ('a,'b) t = {m: ('a*'b) list, eq: 'a*'a->bool}

  fun empty (eq: 'a * 'a -> bool) : ('a,'b)t = {m=nil,eq=eq}

  fun empty_eq () : (''a,'b)t = empty (op =)

  fun lookup (t: ('a,'b)t) (k:'a) : 'b option =
      Option.map (#2) (List.find (fn (q,_) => #eq t(k,q)) (#m t))

  fun rem (k:'a) (t:('a,'b)t) : ('a,'b)t =
      {eq= #eq t, m=List.filter (fn (q,_) => not(#eq t(q,k))) (#m t)}

  fun add (k,v) (t:('a,'b)t) : ('a,'b)t =
      let val t = rem k t
      in {eq= #eq t, m= (k,v) :: (#m t)}
      end

  fun fold (f : 'b*'c -> 'c) (acc:'c) (t:('a,'b)t) : 'c =
      List.foldr (fn ((_,v),a) => f(v,a)) acc (#m t)

  fun Fold (f : ('a*'b)*'c -> 'c) (acc:'c) (t:('a,'b)t) : 'c =
      List.foldr (fn (p,a) => f(p,a)) acc (#m t)

  fun toString (pd: 'a -> string, pr :'b -> string) (t:('a,'b)t) :string =
      "{" ^
      String.concatWith "," (Fold (fn ((a,b),c) => (pd a ^ ":" ^ pr b) :: c) nil t) ^
      "}"

  fun fromList (eq:'a*'a->bool) (l:('a*'b)list) : ('a,'b)t =
      List.foldr (fn ((a,b),m) => add (a,b) m) (empty eq) l

  fun fromList_eq (l:(''a*'b)list) : (''a,'b)t =
      fromList (op =) l

  fun keys (t:('a,'b)t) : 'a list =
      List.map #1 (#m t)

  fun toList (t:('a,'b)t) : ('a*'b)list = #m t
end
