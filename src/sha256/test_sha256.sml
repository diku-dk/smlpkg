(* test case verified at https://timestampgenerator.com/tools/sha256-generator *)
fun try s =
    print (s ^ " -> " ^ SHA256.hashStringHex s ^ "\n")

val () = try "hi"

val () =
    if "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4" =
       SHA256.hashStringHex "hi"
    then print "test: OK\n"
    else print "test: ERR\n"
