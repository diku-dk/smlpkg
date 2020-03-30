
structure Bytestring :> BYTESTRING =
   struct

      type string = Word8Vector.vector
      type byte = Word8.word
      type char = byte

      structure V = Word8Vector
      
      val maxSize = V.maxLen

      val size = V.length

      val sub = V.sub

      fun substring (s, start, len) =
         Word8VectorSlice.vector (Word8VectorSlice.slice (s, start, SOME len))

      fun extract (s, start, leno) =
         Word8VectorSlice.vector (Word8VectorSlice.slice (s, start, leno))

      fun isEmpty str = V.length str = 0

      fun op ^ (s1, s2) = V.concat [s1, s2]

      val concat = V.concat

      val null = V.fromList []

      fun str b = V.fromList [b]

      val implode = V.fromList

      fun explode s = V.foldr (op ::) nil s

      fun map f s = V.map f s

      fun map2 f (s1, s2) =
         if V.length s1 <> V.length s2 then
            raise ListPair.UnequalLengths
         else
            V.mapi (fn (i, b1) => f (b1, V.sub (s2, i))) s1

      fun rev s =
         let
            val len = size s
         in
            V.tabulate (size s, (fn i => V.sub (s, len-i-1)))
         end

      fun eq (s1, s2) =
         let
            val len = size s1

            fun loop i =
               if i >= len then
                  true
               else
                  V.sub (s1, i) = V.sub (s2,i)  andalso  loop (i+1)
         in
            size s2 = len  andalso  loop 0
         end

      fun compare (s1, s2) =
         let
            val len1 = size s1
            val len2 = size s2

            fun loop i =
               if i >= len1 then
                  if i >= len2 then
                     EQUAL
                  else
                     LESS
               else
                  if i >= len2 then
                     GREATER
                  else
                     (case Word8.compare (V.sub (s1, i), V.sub (s2, i)) of
                         EQUAL =>
                            loop (i+1)
                       | x => x)
         in
            loop 0
         end



      fun fromWord8Vector s = s

      fun toWord8Vector s = s


      val fromString = Byte.stringToBytes

      val toString = Byte.bytesToString

      fun fromStringHex str =
         let
            val len = String.size str

            fun loop acc i =
               if i = len then
                  SOME (V.fromList (List.rev acc))
               else if i + 2 > len then
                  NONE
               else
                  (case FromString.scanSubstringAll (Word8.scan StringCvt.HEX) (Substring.substring (str, i, 2)) of
                      NONE => NONE
                    | SOME b =>
                         loop (b :: acc) (i+2))
         in
            loop [] 0
         end

      val ch0 = Char.ord #"0"
      val cha = Char.ord #"a" - 10

      fun nibblestr (n:byte) =
         if n <= 0w9 then
            String.str (Char.chr (Word8.toInt n + ch0))
         else
            String.str (Char.chr (Word8.toInt n + cha))

      fun toStringHex' sep s =
         if V.length s = 0 then
            ""
         else
            let
               val b = Word8Vector.sub (s, 0)
            in
               String.concat
               (nibblestr (Word8.>> (b, 0w4))
                :: nibblestr (Word8.andb (b, 0wxf))
                :: Word8VectorSlice.foldr
                      (fn (b, l) =>
                          sep
                          :: nibblestr (Word8.>> (b, 0w4))
                          :: nibblestr (Word8.andb (b, 0wxf))
                          :: l)
                      []
                      (Word8VectorSlice.slice (s, 1, NONE)))
            end

      val toStringHex = toStringHex' ""

   end
