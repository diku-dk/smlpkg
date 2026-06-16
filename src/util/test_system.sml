val () = println "Testing System"

open System

val cmd = "cat testfile.txt"
val (status,out,err) = command cmd

val () = test "system.ok.out" (out = "Hi there\n")
val () = test "system.ok.err" (err = "")
val () = test "system.ok.status" (OS.Process.isSuccess status)

val cmd_err = "cat doesnotexist.txt"
val (status_err,out_err,err_err) = command cmd_err

val () = test "system.err.out" (out_err = "")
val () = test "system.err.err" (err_err = "cat: doesnotexist.txt: No such file or directory\n")
val () = test "system.err.status" (not(OS.Process.isSuccess status_err))
