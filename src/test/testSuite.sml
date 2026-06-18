functor TestSuite () =
struct

val errors = ref 0;

fun println s = print (s ^ "\n")

fun reportOk s = println ("OK: " ^ s)

fun reportErr s = (errors := !errors + 1; println ("ERR: " ^ s))

fun reportExn s e = (errors := !errors + 1;
                     println ("EXN: " ^ s ^ " raised Fail \"" ^ e ^ "\""))

fun reportExn' s exn_name = (errors := !errors + 1;
                             println ("EXN: " ^ s ^ " raised " ^ exn_name))

fun test s b = (if b then reportOk s else reportErr s)
               handle Fail e => reportExn s e

fun testf s f =
    (if f() then reportOk s else reportErr s)
    handle Fail e => reportExn s e
         | Overflow => reportExn' s "Overflow"
         | _ => reportExn' s "Other"

fun reportAndExit () =
    if !errors > 0
    then OS.Process.exit OS.Process.failure
    else () (* Do not exit on success, for polymlb *)

end;
