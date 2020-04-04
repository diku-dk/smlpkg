
open SemVer

fun test s b =
    if b then print ("OK : " ^ s ^ "\n")
    else print ("ERR: " ^ s ^ "\n")

fun testf s f =
    (if f() then print ("OK : " ^ s ^ "\n")
     else print ("ERR: " ^ s ^ "\n"))
    handle Fail e => print ("EXN: " ^ s ^ " raised Fail \"" ^ e ^ "\"\n")
         | Overflow => print ("EXN: " ^ s ^ " raised Overflow\n")

fun test_major_minor_patch s (z,a,b) =
    let val () = test ("major0" ^ s) (Option.map major (fromString z) = SOME 0)
        val () = test ("major1" ^ s) (Option.map major (fromString a) = SOME 3)
        val () = test ("major2" ^ s) (Option.map major (fromString b) = SOME 10)
        val () = test ("minor0" ^ s) (Option.map minor (fromString z) = SOME 0)
        val () = test ("minor1" ^ s) (Option.map minor (fromString a) = SOME 2)
        val () = test ("minor2" ^ s) (Option.map minor (fromString b) = SOME 40)
        val () = test ("patch0" ^ s) (Option.map patch (fromString z) = SOME 0)
        val () = test ("patch1" ^ s) (Option.map patch (fromString a) = SOME 1)
        val () = test ("patch2" ^ s) (Option.map patch (fromString b) = SOME 121)
        val () = test ("patch0" ^ s) (Option.map patch (fromString z) = SOME 0)
        val () = test ("patch1" ^ s) (Option.map patch (fromString a) = SOME 1)
        val () = test ("patch2" ^ s) (Option.map patch (fromString b) = SOME 121)
    in ()
    end

val z = "0.0.0"
val a = "3.2.1"
val b = "10.40.121"

val zp = "0.0.0-a2"
val ap = "3.2.1-23.a23"
val bp = "10.40.121-yt.43.re-s"

val zpb = "0.0.0-a2+dfd34"
val apb = "3.2.1-23.a23+23.23.4f"
val bpb = "10.40.121-yt.43.re-s+er23.34"

val zb = "0.0.0+dfd34"
val ab = "3.2.1+23.23.4f"
val bb = "10.40.121+er23.34"

val () = test_major_minor_patch "" (z,a,b)
val () = test_major_minor_patch "p" (zp,ap,bp)
val () = test_major_minor_patch "b" (zb,ab,bb)
val () = test_major_minor_patch "pb" (zpb,apb,bpb)

fun test_toString s (z,a,b) =
    let val () = test ("toString.z"^s) (Option.map toString (fromString z) = SOME z)
        val () = test ("toString.a"^s) (Option.map toString (fromString a) = SOME a)
        val () = test ("toString.b"^s) (Option.map toString (fromString b) = SOME b)
    in ()
    end

val () = test_toString "" (z,a,b)
val () = test_toString "p" (zp,ap,bp)
val () = test_toString "pb" (zpb,apb,bpb)
val () = test_toString "b" (zb,ab,bb)

fun test_prerel_empty s (z,a,b) =
    let val () = test ("prerel0"^s) (Option.map prerel (fromString z) = SOME [])
        val () = test ("prerel1"^s) (Option.map prerel (fromString a) = SOME [])
        val () = test ("prerel2"^s) (Option.map prerel (fromString b) = SOME [])
    in ()
    end

val () = test_prerel_empty "" (z,a,b)
val () = test_prerel_empty "b" (zb,ab,bb)

fun test_prerel s (zp,ap,bp) =
    let val () = test ("prerel0p" ^ s) (Option.map prerel (fromString zp) = SOME [ALPHAID "a2"])
        val () = test ("prerel1p" ^ s) (Option.map prerel (fromString ap) = SOME [NUMID 23,ALPHAID"a23"])
        val () = test ("prerel2p" ^ s) (Option.map prerel (fromString bp) = SOME [ALPHAID"yt",NUMID 43,ALPHAID"re-s"])
    in ()
    end

val () = test_prerel "" (zp,ap,bp)
val () = test_prerel "b" (zpb,apb,bpb)

fun lt (s1,s2) =
    case (fromString s1, fromString s2) of
        (SOME a, SOME b) => a < b
      | _ => false

fun notlt (s1,s2) =
    case (fromString s1, fromString s2) of
        (SOME a, SOME b) => not(a < b)
      | _ => false

val () = test "lt_major0" (lt("0.1.0","1.2.0") andalso notlt("1.2.0","0.1.0"))
val () = test "lt_major1" (lt("9.1.0","10.2.0") andalso notlt("10.2.0","9.1.0"))
val () = test "lt_major2" (notlt("1.2.0","1.2.0"))
val () = test "lt_major3" (notlt("20.2.0","3.2.0") andalso lt("3.2.0","20.2.0"))

val () = test "lt_minor0" (lt("1.1.0","1.2.0") andalso notlt("1.2.0","1.1.0"))
val () = test "lt_minor1" (lt("10.9.0","10.12.0") andalso notlt("10.12.0","10.9.0"))
val () = test "lt_minor2" (notlt("1.2.0","1.1.0") andalso lt("1.1.0","1.2.0"))
val () = test "lt_minor3" (notlt("2.20.0","2.8.0") andalso lt("2.8.0","2.20.0"))

val () = test "lt_patch0" (lt("1.1.0","1.1.2") andalso notlt("1.1.2","1.1.0"))
val () = test "lt_patch1" (lt("10.9.2","10.9.12") andalso notlt("10.9.12","10.9.2"))
val () = test "lt_patch2" (notlt("1.1.2","1.1.2"))
val () = test "lt_patch3" (notlt("10.9.12","10.9.8") andalso lt("10.9.8","10.9.12"))

val () = test "lt1" (lt("1.0.0","2.0.0") andalso lt("2.0.0","2.1.0")
                     andalso lt("2.1.0","2.1.1"))

val () = test "lt2" (lt("1.0.0-alpha","1.0.0") andalso notlt("1.0.0","1.0.0-alpha"))

val () = test "lt3" (lt("1.0.0-alpha","1.0.0-alpha.1") andalso notlt("1.0.0-alpha.1","1.0.0-alpha"))

val () = test "lt4" (lt("1.0.0-alpha.1","1.0.0-alpha.beta") andalso notlt("1.0.0-alpha.beta","1.0.0-alpha.1"))
val () = test "lt5" (lt("1.0.0-alpha.beta","1.0.0-beta"))
val () = test "lt6" (lt("1.0.0-beta","1.0.0-beta.2"))
val () = test "lt7" (lt("1.0.0-beta.2","1.0.0-beta.11"))
val () = test "lt8" (lt("1.0.0-beta.11","1.0.0-rc.1"))
val () = test "lt9" (lt("1.0.0-rc.1","1.0.0"))

fun testerr s v = test s (fromString s = NONE)

val () = testerr "majorerr1" "0a.1.2"
val () = testerr "majorerr2" "a0.1.2"

val () = testerr "minorerr1" "0.1a.2"
val () = testerr "minorerr2" "0.a1.2"

val () = testerr "patcherr1" "0.1.2a"
val () = testerr "patcherr2" "0.1.a2"

val () = testerr "prerelerr1" "0.1.2-s*"
val () = testerr "prerelerr2" "0.1.2-*s"

val () = testerr "prerelerr1" "0.1.2-ssd+*l"
val () = testerr "prerelerr2" "0.1.2-ssd+s*"

val () = testf "ovf" (fn () =>
                         let val t = fromString "0.0.0-20180801102532+b70028521e4dbcc286834b32ce82c1d2721a6209"
                         in Option.map major t = SOME 0
                         end)
