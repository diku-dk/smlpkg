(* Semantic versioning; see https://semver.org/ *)

signature SEMVER = sig

  datatype id = NUMID of IntInf.int | ALPHAID of string
  val idToString : id -> string
  val idsToString : id list -> string

  eqtype t
  val fromString : string -> t option
  val toString : t -> string

  val < : t * t -> bool

  val major : t -> int
  val minor : t -> int
  val patch : t -> int
  val prerel : t -> id list
  val build  : t -> id list
end
