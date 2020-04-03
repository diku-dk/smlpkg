signature MANIFEST = sig
  type t
  type pkgpath = {host:string,owner:string,repo:string}
  type semver = SemVer.t
  type hash = string
  type filename = string

  type required = pkgpath * semver * hash option

  val pkgpathToString     : pkgpath -> string
  val pkgpathFromString   : string -> pkgpath option

  val package             : t -> pkgpath option
  val requires            : t -> required list

  val toString            : t -> string                        (* for saving *)
  val fromString          : filename -> string -> t            (* may raise Fail *)
  val fromFile            : filename -> t                      (* may raise Fail *)

  val empty               : pkgpath option -> t

  val add_required        : required -> t -> t
  val del_required        : pkgpath -> t -> t
  val get_required        : t -> pkgpath -> required option
  val replace_requires    : t -> required list -> t

  val pkg_dir             : t -> string option                 (* directory containing package files *)

  val smlpkg_filename     : unit -> string
  val set_smlpkg_filename : string -> unit

  val isCommitVersion     : semver -> string option            (* returns the build id *)
  val commitVersion       : string -> string -> semver option

end
