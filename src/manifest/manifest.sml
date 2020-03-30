structure Manifest :> MANIFEST = struct

structure R = Region
structure L = Lex
structure P = ParseComb(type token = L.token
                        val pr_token = L.pr_token)

type pkgpath = {host:string,owner:string,repo:string}

fun pr_pkgpath (p:pkgpath) : string =
    #host p ^ "/" ^ #owner p ^ "/" ^ #repo p

type semver = SemVer.t
type hash = string
type filename = string
type reg = R.reg

type t = {package: pkgpath option,
          requires: (pkgpath * semver * hash) list}

fun package (m: t) : pkgpath option = #package m
fun requires (m: t) : (pkgpath * semver * hash) list = #requires m

fun pr_require (p,v,h) =
    "  " ^ pr_pkgpath p ^  " " ^ SemVer.toString v ^ " #" ^ h

fun toString (m: t) : string =
    (case #package m of
         SOME p => "package " ^ pr_pkgpath p ^ "\n\n"
       | NONE => "") ^
    ("require {\n") ^
    (String.concatWith "\n" (map pr_require (#requires m))) ^
    ("}")

fun empty (p: pkgpath option) : t =
    {package=p, requires=nil}

local
  open P infix >>> ->> >>- ?? ??? || oo oor
in
(* p_id : string p *)
fun p_id nil = NO (R.botloc, fn () => "expecting identifier but found end-of-file")
  | p_id ((L.Id id,r)::ts) = OK(id,r,ts)
  | p_id ((t,r:reg)::_) = NO (#1 r, fn () => ("expecting identifier but found token " ^ L.pr_token t))

(* p_symb : token p *)
fun p_symb nil = NO (R.botloc,fn () => "reached end-of-file")
  | p_symb ((t,r:reg)::ts) =
    case t of
        L.Symb _ => OK(t,r,ts)
      | _ => NO (#1 r,
                 fn () => ("expecting symbol but found token " ^
                           L.pr_token t))

val p_pkgpath : pkgpath p =
    ((p_id >>- eat (L.Symb #"/")) >>>
     (p_id >>- eat (L.Symb #"/")) >>> p_id) oo
        (fn ((a,b),c) => {host=a,owner=b,repo=c})

val p_hash : string p = p_id

fun p_semver nil = NO (R.botloc, fn () => "expecting semantic version but found end-of-file")
  | p_semver ((L.Id id,r)::ts) =
    (case SemVer.fromString id of
         SOME v => OK(v,r,ts)
       | NONE => NO (#1 r, fn () => "expecting semantic version"))
  | p_semver ((t,r:reg)::_) = NO (#1 r, fn () => ("expecting semantic version but found token " ^ L.pr_token t))

val p_req : (pkgpath * semver * hash) p =
    (p_pkgpath >>> p_semver >>> p_hash) oo (fn ((a,b),c) => (a,b,c))

val rec p_reqs : (pkgpath * semver * hash) list p =
 fn ts =>
    ((eat (L.Symb #"}") oo (fn () => nil))
         || ((p_req oo (fn a => [a]) ?? p_reqs) (op @))
    ) ts

val p_requires : (pkgpath * semver * hash) list p =
    eat (L.Id "requires") ->> eat (L.Symb #"{") ->> p_reqs

val p : t p =
    ((((eat (L.Id "package") ->> p_pkgpath) oo SOME) >>> p_requires)
         || (p_requires oo (fn r => (NONE,r))))
        oo (fn (p,rs) => {package=p,requires=rs})

fun parse (ts:(token*reg) list) : t =
    case p ts of
        OK (v,_,_) => v
      | NO (loc,f) => raise Fail ("parse error at location "
                                  ^ R.ppLoc loc ^ ": " ^ f())
end

fun fromString (filename:string) (content:string) : t =
    let val ts = L.lex filename content
    in parse ts
    end

fun add_requirement (pkgpath,semver,hash) t = t
fun del_requirement (pkgpath,semver) t = t
fun get_requirement t pkgpath = NONE

end
