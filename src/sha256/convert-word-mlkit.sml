
structure ConvertWord : sig
  val word8ToWord32  : Word8.word -> Word32.word
  val word32ToBytesB : Word32.word -> Bytestring.string
  val bytesToWord32B : Bytestring.string -> Word32.word
end =
   struct

      (* This stuff all depends on the size of LargeWord. *)

      exception ConvertWord

      fun word8ToWord32 w = Word32.fromLarge (Word8.toLarge w)

      fun word32ToBytesB w =
         let
            val a = Word8Array.array (4, 0w0)
         in
            PackWord32Big.update (a, 0, Word32.toLarge w);
            Bytestring.fromWord8Vector (Word8Array.vector a)
         end

      fun bytesToWord32B s =
         if Bytestring.size s <> 4 then
            raise ConvertWord
         else
            Word32.fromLarge (PackWord32Big.subVec (Bytestring.toWord8Vector s, 0))

   end
