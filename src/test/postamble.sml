(*
   test postamble

   Include this file at the end of your test suite to report test errors
   to the OS.

   The main function needs to be defined for polymlb to create an executable.
   If any tests fail, the creation of the executable itself will fail with a
   nonzero exit code.

   If the tests pass, the executable itself is a no-op under PolyML.

   Under MLton and MLkit, the test executable, once compiled, will run the
   tests and report any errors to the OS.

*)

fun main () =
    if !errors > 0 then OS.Process.exit(OS.Process.failure) else ()

val _ = main ()
