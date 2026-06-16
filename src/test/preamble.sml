(*
   test preamble

   Defines common test functions and contains an imperative method
   of counting test errors.

   Include this before your test suite, and add postamble.sml at the end

   e.g.
   ../test/preamble.sml
   test_system.sml
   ../test/postamble.sml

*)


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
