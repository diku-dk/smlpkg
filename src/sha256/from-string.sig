
signature FROM_STRING =
   sig

      type cs
      val scanStringAll : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> 'a option
      val scanSubstringAll : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> Substring.substring -> 'a option

      val toInt : string -> int option
      val toWord8 : string -> Word8.word option
      val toWord8Hex : string -> Word8.word option

   end
