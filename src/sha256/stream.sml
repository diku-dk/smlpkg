
structure Stream 
   :> STREAM
   =
   struct

      open Susp

      datatype 'a front = Nil | Cons of 'a * 'a stream
      withtype 'a stream = 'a front susp

      val front = force
      fun eager f = delay (fn () => f)
      val lazy = delay

      fun fromProcess f = 
          lazy
          (fn () => 
                 (case f () of
                     NONE =>
                        Nil
                   | SOME x =>
                        Cons (x, fromProcess f)))

      fun fromList l =
          lazy
          (fn () => 
                 (case l of
                     [] => Nil
                   | h :: t => Cons (h, fromList t)))

      fun fromLoop f seed =
          lazy
          (fn () =>
              (case f seed of
                  NONE =>
                     Nil
                | SOME (seed', x) =>
                     Cons (x, fromLoop f seed')))

      fun fromTable sub table i =
          lazy
          (fn () =>
              (Cons (sub (table, i), fromTable sub table (i+1))
               handle Subscript => Nil))

             

      fun fromString str = fromTable String.sub str 0

      fun fromBytestring str = fromTable Bytestring.sub str 0

      fun fromTextInstream ins =
          fromProcess (fn () => TextIO.input1 ins)

      fun fix f = f (lazy (fn () => front (fix f)))

      exception Empty

      fun hd s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (x, _) =>
                 x)

      fun tl s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (_, s') =>
                 s')

       
      fun op @ (s1, s2) =
          lazy
          (fn () =>
              (case front s1 of
                  Nil =>
                     front s2
                | Cons (h, t) =>
                     Cons (h, t @ s2)))


      fun take (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             []
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    x :: take (s', n-1))

      fun drop (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             s
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    drop (s', n-1))

      fun map f s =
          lazy
          (fn () =>
                 (case front s of
                     Nil =>
                        Nil
                   | Cons (x, s') =>
                        Cons (f x, map f s')))

      fun app f s =
         (case front s of 
             Nil => ()
           | Cons (x, s') => (f x; app f s'))


      fun fold f x s =
         (case front s of
             Nil => x
           | Cons (h, t) =>
                f (h, Susp.delay (fn () => fold f x t)))

   end
