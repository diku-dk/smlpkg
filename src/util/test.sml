val () = println "Testing FinMapEq"

open FinMapEq

val m : (int,string)t = empty_eq()

val m1 = add (5, "five") m
val m2 = add (8, "eight") m1

val toS : (int,string)t -> string = toString (Int.toString,fn s=>s)

val () = test "empty-toString" (toS m = "{}")
val () = test "add1-toString" (toS m1 = "{5:five}")
val () = test "add2-toString" (toS m2 = "{8:eight,5:five}")
