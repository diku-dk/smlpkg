signature SOLVE = sig

  type pkgpath = Manifest.pkgpath
  type semver = SemVer.t
  type buildlist = (pkgpath,semver) FinMapEq.t
  type hash = string

  type pkg_rev_deps = (pkgpath,(semver*hash option)) FinMapEq.t

  val pkgRevDeps : Manifest.t -> pkg_rev_deps
  val solveDeps  : pkg_rev_deps -> buildlist

end
