functor Solve (PI :
               sig
                 type pkg_revinfo
                 val lookupPackageRev  : Manifest.pkgpath -> SemVer.t -> pkg_revinfo
                 val pkgRevGetManifest : pkg_revinfo -> Manifest.t
                 val pkgRevCommit      : pkg_revinfo -> string
               end) : SOLVE =
struct

structure M = FinMapEq
type pkgpath = Manifest.pkgpath
type semver = SemVer.t
type hash = string
type pkg_rev_deps = (pkgpath,(semver*hash option))M.t
type buildlist = (pkgpath,semver) M.t

fun fail s = raise Fail s

fun insert (nil,y) = [y]
  | insert (x::xs,y) = if x=y then x::xs else x::insert(xs,y)
fun merge (xs,nil) = xs
  | merge (xs,y::ys) = merge(insert(xs,y),ys)

fun solve (ps:pkgpath list) (d: pkg_rev_deps) : pkg_rev_deps =
    case ps of
        nil => d
      | p::ps =>
        case M.lookup d p of
            SOME (v,hopt) =>
            let val m = PI.pkgRevGetManifest (PI.lookupPackageRev p v)
                val rs = List.filter (fn (p,v,hopt) =>
                                         case M.lookup d p of
                                             SOME (v',hopt') => SemVer.<(v',v)
                                           | NONE => true)
                                     (Manifest.requires m)
                val d = List.foldl (fn ((p,v,hopt),d) => M.add (p,(v,hopt)) d) d rs
            in solve (merge(ps,map #1 rs)) d
            end
          | NONE =>
            fail "solve internal error - missing pkgpath in dependency map."

fun prune (ps:pkgpath list) (d:pkg_rev_deps) (bl:buildlist) : buildlist =
    case ps of
        nil => bl
      | p::ps =>
        case M.lookup d p of
            SOME (v,hopt) =>
            let val ri = PI.lookupPackageRev p v
                val () = case hopt of
                             SOME h =>
                             if h = PI.pkgRevCommit ri then ()
                             else fail ("Package " ^ Manifest.pkgpathToString p ^
                                        " " ^ SemVer.toString v ^
                                        " has commit hash " ^ PI.pkgRevCommit ri ^
                                        ", but expected " ^ h ^ " from package manifest.")
                           | NONE => ()
                val m = PI.pkgRevGetManifest ri
                val ps' = List.filter (fn p => case M.lookup bl p of
                                                   SOME _ => false
                                                 | NONE => true)
                                      (map #1 (Manifest.requires m))
            in prune (merge(ps',ps)) d (M.add (p,v) bl)
            end
          | NONE =>
            fail "prune internal error - missing pkgpath in dependency map."

fun solveDeps (d: pkg_rev_deps) : buildlist =
    let val roots = M.keys d
        val d = solve roots d           (* gradually solve dependencies *)
    in prune roots d (M.empty_eq())     (* prune packages that are no longer
                                         * in the transitive dependencies *)
    end

fun pkgRevDeps (m:Manifest.t) : pkg_rev_deps =
    List.foldr (fn ((p,v,h),t) => M.add (p,(v,h)) t)
               (M.empty_eq()) (Manifest.requires m)

fun buildListToString (bl: buildlist) : string =
    M.toString (Manifest.pkgpathToString, SemVer.toString) bl


end
