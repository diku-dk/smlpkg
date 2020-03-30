
(* A string-like interface for Word8Vector. *)

signature BYTESTRING =
   sig

      type string = Word8Vector.vector
      type byte = Word8.word
      type char = byte

      val maxSize : int
      val size : string -> int
      val sub : string * int -> byte
      val extract : string * int * int option -> string
      val substring : string * int * int -> string
      val isEmpty : string -> bool
      val ^ : string * string -> string
      val concat : string list -> string
      val null : string
      val str : byte -> string
      val implode : byte list -> string
      val explode : string -> byte list
      val map : (byte -> byte) -> string -> string
      val map2 : (byte * byte -> byte) -> string * string -> string
      val rev : string -> string
      val eq : string * string -> bool
      val compare : string * string -> order

      val fromWord8Vector : Word8Vector.vector -> string
      val fromString : String.string -> string
      val fromStringHex : String.string -> string option

      val toWord8Vector : string -> Word8Vector.vector
      val toString : string -> String.string
      val toStringHex : string -> String.string
      val toStringHex' : String.string -> string -> String.string

   end
