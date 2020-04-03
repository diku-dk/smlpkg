
fun println s = print (s ^ "\n")

val () = println "Testing Solve"

fun test s f =
    (if f() then print ("OK : " ^ s ^ "\n")
     else print ("ERR: " ^ s ^ "\n"))
    handle Fail e => print ("EXN: " ^ s ^ " raised Fail \"" ^ e ^ "\"\n")

fun no_version p v =
    raise Fail ("cannot find version " ^ v ^ " for package " ^ p ^ ".")

(* m_repo10_0_1_X : leaf package with no requirements *)
val m_repo10_0_1_0 =
    Manifest.fromString "m_repo10_0_1_0.pkg"
                        "package github.com/owner1/repo10 require {}"

val m_repo10_0_1_1 =
    Manifest.fromString "m_repo10_0_1_1.pkg"
                        "package github.com/owner1/repo10 require {}"

(* m_repo20_0_2_0 : package with repo10_0_1_0 requirement *)
val m_repo20_0_2_0 =
    Manifest.fromString "m_repo20_0_2_0.pkg"
                        "package github.com/owner2/repo20 require { github.com/owner1/repo10 0.1.0 }"

(* m_repo20_0_2_1 : package with repo10_0_1_1 requirement *)
val m_repo20_0_2_1 =
    Manifest.fromString "m_repo20_0_2_1.pkg"
                        "package github.com/owner2/repo20 require { github.com/owner1/repo10 0.1.1 }"

(* m_repo30_0_3_0 : package with repo20_0_2_0 requirement and repo10_0_1_0 requirement *)
val m_repo30_0_3_0 =
    Manifest.fromString "m_repo30_0_3_0.pkg"
                        "package github.com/owner3/repo30 require { github.com/owner1/repo10 0.1.0 github.com/owner2/repo20 0.2.0 }"

(* m_repo30_0_3_1 : package with repo20_0_2_1 requirement and repo10_0_1_0 requirement *)
val m_repo30_0_3_1 =
    Manifest.fromString "m_repo30_0_3_1.pkg"
                        "package github.com/owner3/repo30 require { github.com/owner1/repo10 0.1.0 github.com/owner2/repo20 0.2.1 }"

structure PkgInfoMock = struct
type pkg_revinfo = Manifest.t * string

fun lookupPackageRev p v =
    case Manifest.pkgpathToString p of
        p as "github.com/owner1/repo10" =>
        (case SemVer.toString v of
             "0.1.0" => (m_repo10_0_1_0, "m_repo10_0_1_0")
           | "0.1.1" => (m_repo10_0_1_1, "m_repo10_0_1_1")
           | v => no_version p v)
      | p as "github.com/owner2/repo20" =>
        (case SemVer.toString v of
             "0.2.0" => (m_repo20_0_2_0, "m_repo20_0_2_0")
           | "0.2.1" => (m_repo20_0_2_1, "m_repo20_0_2_1")
           | v => no_version p v)
      | p as "github.com/owner3/repo30" =>
        (case SemVer.toString v of
             "0.3.0" => (m_repo30_0_3_0, "m_repo30_0_3_0")
           | "0.3.1" => (m_repo30_0_3_1, "m_repo30_0_3_1")
           | v => no_version p v)
      | p => raise Fail ("no package " ^ p)

fun pkgRevGetManifest (pr:pkg_revinfo) : Manifest.t = #1 pr
fun pkgRevCommit (pr:pkg_revinfo) : string = #2 pr
end

structure Solve = Solve (PkgInfoMock)

fun solveManifest (m:Manifest.t) : string =
    let val d = Solve.pkgRevDeps m
        val bl = Solve.solveDeps d
    in Solve.buildListToString bl
    end

val () = test "solve1" (fn () => solveManifest m_repo10_0_1_0 = "{}")

val () = test "solve2" (fn () =>
                           let val res = solveManifest m_repo20_0_2_0
                               val () = println ("   result = " ^ res)
                           in res = "{github.com/owner1/repo10:0.1.0}"
                           end)

val () = test "solve3" (fn () =>
                           let val res = solveManifest m_repo20_0_2_1
                               val () = println ("   result = " ^ res)
                           in res = "{github.com/owner1/repo10:0.1.1}"
                           end)

val () = test "solve4" (fn () =>
                           let val res = solveManifest m_repo30_0_3_0
                               val () = println ("   result = " ^ res)
                           in res = "{github.com/owner2/repo20:0.2.0,github.com/owner1/repo10:0.1.0}"
                           end)

val () = test "solve5" (fn () =>
                           let val res = solveManifest m_repo30_0_3_1
                               val () = println ("   result = " ^ res)
                           in res = "{github.com/owner2/repo20:0.2.1,github.com/owner1/repo10:0.1.1}"
                           end)
