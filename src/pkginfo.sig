signature PKG_INFO = sig

  type pkgpath = Manifest.pkgpath
  type semver = SemVer.t

  type pkg_revinfo
  val pkgRevRepoUrl       : pkg_revinfo -> string
  val pkgRevRef           : pkg_revinfo -> string
  val pkgRevCommit        : pkg_revinfo -> string
  val pkgRevGetManifest   : pkg_revinfo -> Manifest.t   (* cached access *)
  val pkgRevTime          : pkg_revinfo -> Time.time

  type pkg_info
  val pkgInfo             : pkgpath -> pkg_info         (* raw - no caching *)
  val pkgVersions         : pkg_info -> (semver,pkg_revinfo) FinMapEq.t

  (* Package registry - cached access *)
  val lookupPackage       : pkgpath -> pkg_info
  val lookupPackageCommit : pkgpath -> string option -> semver * pkg_revinfo
  val lookupPackageRev    : pkgpath -> semver -> pkg_revinfo
  val lookupNewestRev     : pkgpath -> semver

  val verboseFlag         : bool ref
end
