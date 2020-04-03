structure Manifest :> MANIFEST = struct

structure R = Region
structure L = Lex
structure P = ParseComb(type token = L.token
                        val pr_token = L.pr_token)

type pkgpath = {host:string,owner:string,repo:string}

fun pkgpathToString (p:pkgpath) : string =
    #host p ^ "/" ^ #owner p ^ "/" ^ #repo p

fun pkgpathFromString (p:string) : pkgpath option =
    case String.fields (fn c => c = #"/") p of
        [host,owner,repo] => SOME {host=host,owner=owner,repo=repo}
      | _ => NONE

type semver = SemVer.t
type hash = string
type filename = string
type reg = R.reg

type required = pkgpath * semver * hash option

type t = {package: pkgpath option,
          requires: required list}

fun package (m: t) : pkgpath option = #package m
fun requires (m: t) : required list = #requires m

fun pr_require (p,v,hopt) =
    "  " ^ pkgpathToString p ^  " " ^ SemVer.toString v ^
    (case hopt of SOME h => " #" ^ h | NONE => "")

fun toString (m: t) : string =
    (case #package m of
         SOME p => "package " ^ pkgpathToString p ^ "\n\n"
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

val p_hash : string p = eat (L.Symb #"#") ->> p_id

fun p_semver nil = NO (R.botloc, fn () => "expecting semantic version but found end-of-file")
  | p_semver ((L.Id id,r)::ts) =
    (case SemVer.fromString id of
         SOME v => OK(v,r,ts)
       | NONE => NO (#1 r, fn () => "expecting semantic version"))
  | p_semver ((t,r:reg)::_) = NO (#1 r, fn () => ("expecting semantic version but found token " ^ L.pr_token t))

val p_req : required p =
    ((p_pkgpath >>> p_semver) oo (fn (a,b) => (a,b,NONE)) ?? p_hash) (fn ((a,b,_),c) => (a,b,SOME c))

val rec p_reqs : required list p =
 fn ts =>
    ((eat (L.Symb #"}") oo (fn () => nil))
         || ((p_req oo (fn a => [a]) ?? p_reqs) (op @))
    ) ts

val p_requires : required list p =
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

fun readAll (filename:string) : string =   (* may raise Fail *)
    let val is = TextIO.openIn filename
    in (TextIO.inputAll is handle _ => (TextIO.closeIn is;
                                        raise Fail ("failed to read file '" ^ filename ^ "'")))
       before TextIO.closeIn is
    end

fun fromFile (filename:string) : t =
    fromString filename (readAll filename)

fun add_required (r:required) (t:t) : t =
    {package= #package t,
     requires= r :: (#requires t)}

fun del_required (pkgpath:pkgpath) (t:t) : t =
    {package= #package t,
     requires= List.filter (fn r => #1 r <> pkgpath) (#requires t)}

fun get_required (t:t) (pkgpath:pkgpath) : required option =
    List.find (fn r => #1 r = pkgpath) (#requires t)

fun replace_requires (t:t) (rs:required list) : t =
    {package= #package t,
     requires=rs}

fun pkg_dir (t:t) : string option =
    case #package t of
        SOME p => SOME ("lib/" ^ pkgpathToString p ^ "/")
      | NONE => NONE

fun smlpkg_filename () = "sml.pkg"

(* Versions of the form (0,0,0)-timestamp+hash are treated
   specially, as a reference to the commit identified uniquely with
   'hash' (typically the Git commit ID).  This function detects such
   versions. *)

fun isCommitVersion (v:semver) : string option =
    case (SemVer.major v, SemVer.minor v, SemVer.patch v) of
        (0,0,0) => SOME (SemVer.idsToString (SemVer.build v))
      | _ => NONE

(* @commitVersion timestamp commit@ constructs a commit version. *)

fun commitVersion (time:string) (commit:string) : semver option =
    SemVer.fromString ("0.0.0-" ^ time ^ "+" ^ commit)

end
