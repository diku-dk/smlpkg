
(* A valid implementation of SUSP, for platforms that don't support suspensions primitively. *)

structure Susp :> SUSP =
   struct

      type 'a susp = (unit -> 'a) ref

      fun delay f =
          let
             val r = ref (fn () => raise (Fail "empty suspension"))
                
             val () =
                r := (fn () => let
                                  val x = f ()
                               in
                                  r := (fn () => x);
                                  x
                               end)
          in
             r
          end

      fun force r = !r ()

   end
