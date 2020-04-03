fun println s = print (s ^ "\n")

val () = println "Testing System"

fun test s b =
    println(if b then ("OK : " ^ s)
            else ("ERR: " ^ s))

open System

val cmd = "wc testfile.txt"
val (status,out,err) = command cmd

val () = test "system.ok.out" (out = "       8      18      88 testfile.txt\n")
val () = test "system.ok.err" (err = "")
val () = test "system.ok.status" (OS.Process.isSuccess status)

val cmd_err = "ls doesnotexist.txt"
val (status_err,out_err,err_err) = command cmd_err

val () = test "system.err.out" (out_err = "")
val () = test "system.err.err" (err_err = "ls: doesnotexist.txt: No such file or directory\n")
val () = test "system.err.status" (not(OS.Process.isSuccess status_err))
