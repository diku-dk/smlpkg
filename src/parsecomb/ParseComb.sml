functor ParseComb(eqtype token
                  val pr_token : token -> string) : PARSE_COMB = struct
type loc = Region.loc
type reg = Region.reg

type token = token

(* keep track of the max location - the longest parse *)
datatype ('a,'b) either = OK of 'a | NO of 'b

type locerr = loc * (unit -> string)
fun maxLocerr (l1:locerr) l2 =
    if Region.lt (#1 l1) (#1 l2) then l2
    else l1

type 'a p = (token*reg)list -> ('a * reg * (token*reg)list, locerr) either

infix >>> ->> >>- ?? ??? || oo oor
fun p1 >>> p2 = fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) => OK((v1,v2), Region.plus ">>>" r1 r2, ts)
           | NO l => NO (maxLocerr l (#2 r1,fn()=>"")))
      | NO l => NO l

fun p1 ->> p2 = fn ts =>
    case p1 ts of
        OK((),r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) => OK(v2, Region.plus "->>" r1 r2, ts)
           | NO l => NO (maxLocerr l (#2 r1,fn()=>"")))
      | NO l => NO l

fun p1 >>- p2 = fn ts =>
    case p1 ts of
        OK(v,r1,ts) =>
        (case p2 ts of
             OK((),r2,ts) => OK(v, Region.plus ">>-" r1 r2, ts)
           | NO l => NO (maxLocerr l (#2 r1,fn()=>"")))
      | NO l => NO l

fun p1 ?? p2 = fn f => fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) => OK(f(v1,v2), Region.plus "??" r1 r2, ts)
           | _ => OK(v1,r1,ts))
      | NO l => NO l

fun p1 ??? p2 = fn f => fn ts =>
    case p1 ts of
        OK(v1,r1,ts) =>
        (case p2 ts of
             OK(v2,r2,ts) => 
             let val r = Region.plus "???" r1 r2
             in OK(f(v1,v2,r), r, ts)
             end
           | NO l => OK(v1,r1,ts))
      | NO l => NO l

fun p1 || p2 = fn ts =>
    case p1 ts of
        OK(v,r,ts) => OK(v,r,ts)
      | NO l1 => case p2 ts of
                    OK(v,r,ts) => OK(v,r,ts)
                  | NO l2 => NO (maxLocerr l1 l2)

fun ign p ts =
    case p ts of
      OK (_,r,ts) => OK ((),r,ts)
    | NO l => NO l

fun p oo f = fn ts =>
    case p ts of
      OK(v,r,ts) => OK(f v,r,ts)
    | NO l => NO l

fun p oor f = fn ts =>
    case p ts of
      OK(v,r,ts) => OK(f(v,r),r,ts)
    | NO l => NO l

fun eat t ts =
    case ts of
      nil => NO (Region.botloc,fn() => ("expecting token " ^ pr_token t ^ 
                                        " but reached end-of-file"))
    | (t',r:Region.reg)::ts' => if t=t' then OK ((),r,ts')
                                else NO (#1 r,fn()=> ("expecting token " ^ pr_token t ^ 
                                                      " but found token " ^ pr_token t'))
end
