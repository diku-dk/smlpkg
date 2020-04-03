signature ZIP = sig

  type t            (* type of downloaded archive *)
  type entry        (* type of entry in archive *)

  val download      : string -> t
  val eRelativePath : entry -> string
  val fromEntry     : entry -> Word8Vector.vector   (* load content *)
  val zEntries      : t -> entry list
end

structure Zip :> ZIP = struct
type t = int
type entry = int

fun not_impl s = raise Fail ("not implemented: " ^ s)

fun download x = not_impl "Zip.download"

fun eRelativePath x = not_impl "Zip.eRelativePath"

fun fromEntry x = not_impl "Zip.fromEntry"

fun zEntries x = not_impl "Zip.zEntries"

end
