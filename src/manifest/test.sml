
val () = println "Testing Manifest"
open Manifest


val m = "require {}"

val () = testf "empty-m" (fn () => null(requires(fromString "str" m)))

val mp = "package github.com/owner/repo require {}"
val () = testf "empty-mp" (fn () => null(requires(fromString "str" mp)))
val () = testf "empty-mp-host" (fn () => SOME "github.com" = Option.map #host (package(fromString "str" mp)))
val () = testf "empty-mp-owner" (fn () => SOME "owner" = Option.map #owner (package(fromString "str" mp)))
val () = testf "empty-mp-repo" (fn () => SOME "repo" = Option.map #repo (package(fromString "str" mp)))

val mr1 = "require { github.com/owner/repo 1.2.3 #asdefsde }"
val () = testf "empty-mr1-len" (fn () => 1 = length(requires(fromString "str" mr1)))
val () = testf "empty-mr1" (fn () => SOME "asdefsde" = #3(List.hd(requires(fromString "str" mr1))))

val mr2 = "require { github.com/owner/repo 1.2.3 #asdefsde github.com/owner2/repo8 43.3.2-alpha #523424abcd }"
val () = testf "empty-mr2" (fn () => 2 = length(requires(fromString "str" mr2)))
val () = testf "empty-mr2-version" (fn () => "1.2.3" = SemVer.toString(#2(List.hd(requires(fromString "str" mr2)))))
