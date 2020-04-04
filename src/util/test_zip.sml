
fun println s = print (s ^ "\n")
val () = println "Testing Zip"

fun test s f =
    (if f() then println ("OK : " ^ s)
     else println ("ERR: " ^ s))
    handle Fail e => println ("EXN: " ^ s ^ " raised Fail \"" ^ e ^ "\"")

val zipfile = "test_zip/v0.1.0.zip"

val z = Zip.fromFile zipfile

val () = Zip.extractSubDir {log=println} z {path="segmented-0.1.0/lib", target="test_zip/lib"}
