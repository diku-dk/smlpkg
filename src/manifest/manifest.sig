signature MANIFEST =
sig
  type t
  type pkgpath = {host:string,owner:string,repo:string}
  type semver = SemVer.t
  type hash = string
  type filename = string

  val package         : t -> pkgpath option
  val requires        : t -> (pkgpath * semver * hash) list

  val toString        : t -> string                        (* for saving *)
  val fromString      : filename -> string -> t            (* for loading; may raise Fail *)

  val empty           : pkgpath option -> t

  val add_requirement : pkgpath * semver * hash -> t -> t
  val del_requirement : pkgpath * semver -> t -> t
  val get_requirement : t -> pkgpath -> (pkgpath * semver * hash) option
end
