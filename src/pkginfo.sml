structure PkgInfo :> PKG_INFO = struct

  structure M = FinMapEq

  fun println s = print (s ^ "\n")
  fun log s = println s

  type pkgpath = Manifest.pkgpath
  type semver = SemVer.t

  type pkg_revinfo =
       {pkgRevZipballUrl  : string,
        pkgRevZipballDir  : string,               (* the directory inside zipball containing the 'lib' dir *)
        pkgRevCommit      : string,               (* commit id for verification *)
        pkgRevGetManifest : unit -> Manifest.t,
        pkgRevTime        : Time.time}

  fun pkgRevZipballUrl (r:pkg_revinfo) : string = #pkgRevZipballUrl r
  fun pkgRevZipballDir (r:pkg_revinfo) : string = #pkgRevZipballDir r
  fun pkgRevCommit (r:pkg_revinfo) : string = #pkgRevCommit r
  fun pkgRevGetManifest (r:pkg_revinfo) : Manifest.t = #pkgRevGetManifest r ()
  fun pkgRevTime (r:pkg_revinfo) : Time.time = #pkgRevTime r

  type pkg_info = {pkgVersions: (semver,pkg_revinfo)M.t,
                   pkgLookupCommit : string option -> pkg_revinfo}

  fun lookupPkgRev (v:semver) (pi:pkg_info) : pkg_revinfo option =
      M.lookup (#pkgVersions pi) v

  fun majorRevOfPkg (p: pkgpath) : pkgpath * int list =
      let fun mk r = {host= #host p, owner= #owner p, repo= r}
      in case String.fields (fn c => c = #"@") (#repo p) of
             [r,v] => (case Int.fromString v of
                           SOME i => if Int.toString i = v then (mk r,[i])
                                     else raise Fail "majorRevOfPkg: expecting integer"
                         | NONE => (mk r,[0,1]))
           | _ => (p,[0,1])
      end

  (* Utilities *)
  fun httpRequest (url:string) : string =
      let val cmd = "curl -L " ^ url
          val (status,out,err) = System.command cmd
      in if OS.Process.isSuccess status then out
         else (TextIO.output(TextIO.stdErr,err);
               raise Fail ("Failed to execute http request using curl: '" ^ cmd ^ "'"))
      end

  fun gitCmd (opts : string list) : string =           (* may raise Fail and print errors on stderr *)
      let val cmd = String.concatWith " " ("git"::opts)
(*
          val () = (* Avoid Git asking for credentials.  We prefer failure. *)
               setEnv "GIT_TERMINAL_PROMPT" "0"
*)
          val (status,out,err) = System.command cmd
      in if OS.Process.isSuccess status then out
         else (TextIO.output(TextIO.stdErr,err);
               raise Fail ("Failed to execute git command '" ^ cmd ^ "'"))
      end

  (* The GitLab and GitHub interactions are very similar, so we define a
     couple of generic functions that are used to implement support for
     both. *)

  fun ghglRevGetManifest (url:string) (owner:string) (repo:string) (tag:string) : Manifest.t =
      let val () = log ("Downloading package manifest from " ^ url)
          val path = owner ^ "/" ^ repo ^ "@" ^ tag ^ "/" ^ Manifest.smlpkg_filename()
          val s = httpRequest url
                  handle Fail e =>
                         raise Fail ("Network error when reading " ^ path ^ ":\n" ^ e)
      in Manifest.fromString path s
      end

  val cache =
      let val m : (string * Manifest.t) list ref = ref nil
      in fn f => fn a => fn b => fn c => fn d => fn () =>
            let val s = String.concatWith "/" [a,b,c,d]
            in case List.find (fn (k,_) => k=s) (!m) of
                   SOME (_,v) => v
                 | NONE => let val v = f a b c d
                           in m := (s,v) :: !m
                            ; v
                           end
            end
      end

  fun ghglLookupCommit (archive_url:string) (manifest_url:string)
                       (owner:string) (repo:string) (d:string)
                       (tag:string) (hash:string) : pkg_revinfo =
      let val mc = cache ghglRevGetManifest manifest_url owner repo tag
          val dir = repo ^ "-" ^ d
          val time = Time.now()
      in {pkgRevZipballUrl=archive_url,
          pkgRevZipballDir=dir,
          pkgRevCommit=hash,
          pkgRevGetManifest=mc,
          pkgRevTime=time}
      end

  fun ghglPkgInfo (repo_url:string) mk_archive_url mk_manifest_url
                  (owner:string) (repo:string) (versions:int list) : pkg_info =
      let val () = log ("Retrieving list of tags from " ^ repo_url)
          val remote_lines = gitCmd ["ls-remote", repo_url]
          val remote_lines = String.tokens (fn c => c = #"\n") remote_lines
          fun isHeadRef (l:string) : string option =
              case String.tokens Char.isSpace l of
                  [hash,"HEAD"] => SOME hash
                | _ => NONE
          fun revInfo l : (semver * pkg_revinfo) option =
              case String.tokens Char.isSpace l of
                  [hash,refe] =>
                  (case String.fields (fn s => s = #"/") refe of
                       ["refs", "tags", t] =>
                       if String.isPrefix "v" t then
                         (case SemVer.fromString(String.extract(t,1,NONE)) of
                              SOME v =>
                              let val m = SemVer.major v
                              in if List.exists (fn i => i=m) versions then
                                   let val pinfo = ghglLookupCommit (mk_archive_url t) (mk_manifest_url t)
                                                                    owner repo (SemVer.toString v) t hash
                                   in SOME (v,pinfo)
                                   end
                                 else NONE
                              end
                            | NONE => NONE)
                       else NONE
                     | _ => NONE)
                | _ => NONE
      in case List.mapPartial isHeadRef remote_lines of
             head_ref :: _ =>
             let fun def (opt:string option) : string = Option.getOpt(opt,head_ref)
                 val rev_info = M.fromList_eq (List.mapPartial revInfo remote_lines)
                 fun lookupCommit (r:string option) =
                     ghglLookupCommit (mk_archive_url (def r)) (mk_manifest_url (def r))
                                      owner repo (def r) (def r) (def r)
             in {pkgVersions=rev_info,
                 pkgLookupCommit=lookupCommit}
             end
           | _ => raise Fail ("Cannot find HEAD ref for " ^ repo_url)
      end

  fun ghPkgInfo (owner:string) (repo:string) (versions:int list) : pkg_info =
      let val repo_url = "https://github.com/" ^ owner ^ "/" ^ repo
          fun mk_archive_url r = repo_url ^ "/archive/" ^ r ^ ".zip"
          fun mk_manifest_url r = "https://raw.githubusercontent.com/" ^
                                  owner ^ "/" ^ repo ^ "/" ^
                                  r ^ "/" ^ Manifest.smlpkg_filename()
      in ghglPkgInfo repo_url mk_archive_url mk_manifest_url owner repo versions
      end

  fun glPkgInfo (owner:string) (repo:string) (versions:int list) : pkg_info =
      let val base_url = "https://gitlab.com/" ^ owner ^ "/" ^ repo
          val repo_url = base_url ^ ".git"
          fun mk_archive_url r = base_url ^ "/-/archive/" ^ r ^
                                 "/" ^ repo ^ "-" ^ r ^ ".zip"
          fun mk_manifest_url r = base_url ^ "/raw/" ^
                                  r ^ "/" ^ Manifest.smlpkg_filename()
      in ghglPkgInfo repo_url mk_archive_url mk_manifest_url owner repo versions
      end

  (* Retrieve information about a package based on its package path.
     This uses Semantic Import Versioning when interacting with
     repositories.  For example, a package @github.com/user/repo@ will
     match version 0.* or 1.* tags only, a package
     @github.com/user/repo/v2@ will match 2.* tags, and so forth..
   *)

  (* Raw access - limited caching *)

  fun pkgInfo (p:pkgpath) : pkg_info =
      case #host p of
          "github.com" =>
          let val (p',vs) = majorRevOfPkg p
          in ghPkgInfo (#owner p) (#repo p') vs
          end
        | _ => raise Fail ("Unable to handle package paths of the form '"
                           ^ Manifest.pkgpathToString p ^ "'")

  fun pkgVersions (pinfo: pkg_info) : (semver,pkg_revinfo) M.t =
      #pkgVersions pinfo


  (* Cached access *)

  local
    val registry : (pkgpath,pkg_info)M.t ref = ref (M.empty_eq())
  in
    fun lookupPackage (p:pkgpath) : pkg_info =
        case M.lookup (!registry) p of
            SOME i => i
          | NONE => let val i = pkgInfo p
                    in registry := M.add (p,i) (!registry)
                     ; i
                    end

    fun lookupPackageCommit (p:pkgpath) (refe:string option) : semver * pkg_revinfo =
        let val pinfo = lookupPackage p
            val rev_info = #pkgLookupCommit pinfo refe
            val timestamp = Date.fmt "%Y%m%d%H%M%S" (Date.fromTimeLocal(pkgRevTime rev_info))
            val v = case Manifest.commitVersion timestamp (pkgRevCommit rev_info) of
                        NONE => raise Fail "impossible: failed to form valid commit version"
                      | SOME v => v
            val pinfo' = {pkgLookupCommit = #pkgLookupCommit pinfo,
                          pkgVersions = M.add (v,rev_info) (#pkgVersions pinfo)}
            val () = registry := M.add (p,pinfo') (!registry)
        in (v, rev_info)
        end

    (* Look up information about a specific version of a package. *)
    fun lookupPackageRev (p:pkgpath) (v:semver) : pkg_revinfo =
        case Manifest.isCommitVersion v of
            SOME commit => #2 (lookupPackageCommit p (SOME commit))
          | NONE =>
            let val pinfo = lookupPackage p
            in case lookupPkgRev v pinfo of
                   NONE =>
                   let val versions =
                           case M.keys (#pkgVersions pinfo) of
                               [] => ("Package " ^ Manifest.pkgpathToString p ^
                                      " has no versions.  Invalid package path?")
                             | ks => ("Known versions: " ^
                                      String.concatWith ", " (map SemVer.toString ks))
                       val (_, vs) = majorRevOfPkg p
                       val major =
                           if List.exists (fn x => x = SemVer.major v) vs then ""
                           else ("\nFor major version " ^ Int.toString (SemVer.major v) ^
                                 ", use package path " ^ Manifest.pkgpathToString p
                                 ^ "@" ^ Int.toString (SemVer.major v))
                   in raise Fail ("package " ^ Manifest.pkgpathToString p ^
                                  " does not have a version " ^ SemVer.toString v ^
                                  ".\n" ^ versions ^ major)
                   end
                 | SOME v' => v'
            end

    (* Find the newest version of a package. *)
    fun lookupNewestRev (p:pkgpath) : semver =
        let val pinfo = lookupPackage p
        in case M.keys (#pkgVersions pinfo) of
               [] =>
               ( log ("Package " ^ Manifest.pkgpathToString p ^
                      " has no released versions.  Using HEAD.")
               ; #1 (lookupPackageCommit p NONE))
             | v::vs =>
               let fun max (v1,v2) = if SemVer.< (v1,v2) then v2 else v1
               in List.foldl max v vs  (* memo: what about versions of
                                        * equal priority - should we use
                                        * foldr? *)
               end
        end
  end
end
