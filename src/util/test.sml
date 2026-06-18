structure T = TestSuite (); open T

val () = println "Testing FinMapEq"
local open FinMapEq in

val m : (int,string)t = empty_eq()

val m1 = add (5, "five") m
val m2 = add (8, "eight") m1

val toS : (int,string)t -> string = toString (Int.toString,fn s=>s)

val () = test "empty-toString" (toS m = "{}")
val () = test "add1-toString" (toS m1 = "{5:five}")
val () = test "add2-toString" (toS m2 = "{8:eight,5:five}")
end


val () = println "Testing System"
local open System in

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
end


val _ = reportAndExit ()
