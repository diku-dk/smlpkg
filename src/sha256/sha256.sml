
structure SHA256 :> sig
      val hashStringHex : string -> string
end = struct

      (* Based on Wikipedia's SHA-256 pseudocode:
         http://en.wikipedia.org/w/index.php?title=SHA-2&oldid=547734565#Pseudocode
      *)

      structure S = Stream
      structure W = Word32
      structure A = Array
      structure B = Bytestring

      val w8to32 = ConvertWord.word8ToWord32


      fun rr (w, i) =
         W.orb (W.>> (w, i),
                W.<< (w, 0w32-i))


      fun implodeWord32 (b3, b2, b1, b0) =
         W.orb (W.<< (w8to32 b3, 0w24),
                W.orb (W.<< (w8to32 b2, 0w16),
                       W.orb (W.<< (w8to32 b1, 0w8),
                              w8to32 b0)))


      fun replicateOnto i x s =
         if i <= 0 then
            s
         else
            S.eager (S.Cons (x, replicateOnto (i-1) x s))


      fun szstream i =
         let
            val sz = W.fromInt i
         in
            S.eager (S.Cons (W.>> (sz, 0w29), 
                             S.eager (S.Cons (W.<< (sz, 0w3), 
                                              S.eager S.Nil))))
         end


      fun pad i =
         let
            val j = (i div 4 + 1) mod 16
         in
            if j <= 14 then
               14-j
            else
               30-j
         end


      (* put into 32-bit chunks and add material to end *)
      fun chunk i s =
         S.lazy
         (fn () =>
             (case S.front s of
                 S.Nil =>
                    S.Cons (0wx80000000,
                            replicateOnto (pad i) 0w0 (szstream i))
               | S.Cons (b3, s3) =>
                    (case S.front s3 of
                        S.Nil =>
                           S.Cons (implodeWord32 (b3, 0wx80, 0w0, 0w0),
                                   replicateOnto (pad i) 0w0 (szstream (i+1)))
                      | S.Cons (b2, s2) =>
                           (case S.front s2 of
                               S.Nil =>
                                  S.Cons (implodeWord32 (b3, b2, 0wx80, 0w0),
                                          replicateOnto (pad i) 0w0 (szstream (i+2)))
                             | S.Cons (b1, s1) =>
                                  (case S.front s1 of
                                      S.Nil =>
                                         S.Cons (implodeWord32 (b3, b2, b1, 0wx80),
                                                 replicateOnto (pad i) 0w0 (szstream (i+3)))
                                    | S.Cons (b0, s0) =>
                                         S.Cons (implodeWord32 (b3, b2, b1, b0),
                                                 chunk (i+4) s0))))))


      type hashstate = W.word * W.word * W.word * W.word * W.word * W.word * W.word * W.word

      val hshInit : hashstate =
         (0wx6a09e667, 0wxbb67ae85, 0wx3c6ef372, 0wxa54ff53a, 0wx510e527f, 0wx9b05688c, 0wx1f83d9ab, 0wx5be0cd19)
      
      val k : W.word Vector.vector =
         Vector.fromList
         [0wx428a2f98, 0wx71374491, 0wxb5c0fbcf, 0wxe9b5dba5, 0wx3956c25b, 0wx59f111f1, 0wx923f82a4, 0wxab1c5ed5,
          0wxd807aa98, 0wx12835b01, 0wx243185be, 0wx550c7dc3, 0wx72be5d74, 0wx80deb1fe, 0wx9bdc06a7, 0wxc19bf174,
          0wxe49b69c1, 0wxefbe4786, 0wx0fc19dc6, 0wx240ca1cc, 0wx2de92c6f, 0wx4a7484aa, 0wx5cb0a9dc, 0wx76f988da,
          0wx983e5152, 0wxa831c66d, 0wxb00327c8, 0wxbf597fc7, 0wxc6e00bf3, 0wxd5a79147, 0wx06ca6351, 0wx14292967,
          0wx27b70a85, 0wx2e1b2138, 0wx4d2c6dfc, 0wx53380d13, 0wx650a7354, 0wx766a0abb, 0wx81c2c92e, 0wx92722c85,
          0wxa2bfe8a1, 0wxa81a664b, 0wxc24b8b70, 0wxc76c51a3, 0wxd192e819, 0wxd6990624, 0wxf40e3585, 0wx106aa070,
          0wx19a4c116, 0wx1e376c08, 0wx2748774c, 0wx34b0bcb5, 0wx391c0cb3, 0wx4ed8aa4a, 0wx5b9cca4f, 0wx682e6ff3,
          0wx748f82ee, 0wx78a5636f, 0wx84c87814, 0wx8cc70208, 0wx90befffa, 0wxa4506ceb, 0wxbef9a3f7, 0wxc67178f2]


      fun hashBatch (hsh, cur) =
         let
            fun doextend i =
               if i >= 64 then
                  ()
               else
                  let
                     val v15 = A.sub (cur, i-15)
                     val v2 = A.sub (cur, i-2)
                     val v16 = A.sub (cur, i-16)
                     val v7 = A.sub (cur, i-7)

                     open W

                     val s0 = xorb (xorb (rr (v15, 0w7), rr (v15, 0w18)), >> (v15, 0w3))
                     val s1 = xorb (xorb (rr (v2, 0w17), rr (v2, 0w19)), >> (v2, 0w10))
                  in
                     A.update (cur, i, v16 + s0 + v7 + s1);
                     doextend (Int.+ (i, 1))
                  end

            fun dostep i (a, b, c, d, e, f, g, h) =
               if i >= 64 then
                  let
                     val (a', b', c', d', e', f', g', h') = hsh
                  in
                     (W.+ (a', a),
                      W.+ (b', b),
                      W.+ (c', c),
                      W.+ (d', d),
                      W.+ (e', e),
                      W.+ (f', f),
                      W.+ (g', g),
                      W.+ (h', h))
                  end
               else
                  let
                     open W
         
                     val s1 = xorb (xorb (rr (e, 0w6), rr (e, 0w11)), rr (e, 0w25))
                     val ch = xorb (andb (e, f), andb (notb e, g))
                     val temp = h + s1 + ch + Vector.sub (k, i) + A.sub (cur, i)
                     val s0 = xorb (xorb (rr (a, 0w2), rr (a, 0w13)), rr (a, 0w22))
                     val maj = xorb (andb (a, xorb (b, c)), andb (b, c))
                  in
                     dostep (Int.+ (i, 1)) (temp+s0+maj, a, b, c, d+temp, e, f, g)
                  end
         in
            doextend 16;
            dostep 0 hsh
         end
      
      
      fun hashStream (len, hsh, s) =
         let
            val cur : W.word A.array = A.array (64, 0w0)

            fun loadBlock i s =
               if i > 15 then
                  s
               else
                  (case S.front s of
                      S.Nil =>
                         raise (Fail "impossible")  (* |s| is a multiple of 16, so this can't happen *)
                    | S.Cons (w, s') =>
                         (
                         A.update (cur, i, w);
                         loadBlock (i+1) s'
                         ))

            fun loop (hsh, s) =
               (case S.front s of
                   S.Nil => hsh
                 | _ =>
                      let
                         val s' = loadBlock 0 s
                      in
                         loop (hashBatch (hsh, cur), s')
                      end)

            val (a, b, c, d, e, f, g, h) = loop (hsh, chunk len s)
         in
            B.concat
            (map ConvertWord.word32ToBytesB [a, b, c, d, e, f, g, h])
         end


      fun hash s = hashStream (0, hshInit, s)

      fun streamFromBytestring str = Stream.fromTable B.sub str 0
      fun streamFromString str = Stream.fromTable (fn ((), i) => Word8.fromInt (Char.ord (String.sub (str, i)))) () 0

      fun hashBytes str = hash (streamFromBytestring str)
      fun hashString str = hash (streamFromString str)


      type state = (int * hashstate * B.string)

      val initial = (0, hshInit, B.null)

      fun update ((len, hsh, accum), str) =
         let
            fun loop (st as (len, hsh, accum)) =
               if B.size accum < 64 then
                  st
               else
                  let
                     val cur : W.word A.array = A.array (64, 0w0)
   
                     fun loadBlock i s =
                        if i > 15 then
                           s
                        else
                           let
                              val h = B.substring (s, 0, 4)
                              val t = B.extract (s, 4, NONE)
                           in
                              A.update (cur, i, ConvertWord.bytesToWord32B h);
                              loadBlock (i+1) t
                           end
                           
                     val accum' = loadBlock 0 accum
   
                     val hshNext = hashBatch (hsh, cur)
                  in
                     loop (len + 64, hshNext, accum')
                  end
         in
            loop (len, hsh, B.^ (accum, str))
         end

      fun finish ((len, hsh, accum), s) =
         hashStream (len, hsh, S.@ (streamFromBytestring accum, s))

      val hashStringHex = Bytestring.toStringHex o hashString

   end
