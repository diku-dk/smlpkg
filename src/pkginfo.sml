structure PkgInfo :> PKG_INFO =
struct

  structure M = FinMapEq

  fun println s = print (s ^ "\n")

  val verboseFlag = ref false
  fun log s = if !verboseFlag then println ("[" ^ s ^ "]") else ()

  type pkgpath = Manifest.pkgpath
  type semver = SemVer.t

  type pkg_revinfo =
       {pkgRevRepoUrl     : string,               (* git repository URL *)
        pkgRevRef         : string,               (* git ref (tag or commit) *)
        pkgRevCommit      : string,               (* commit id for verification *)
        pkgRevGetManifest : unit -> Manifest.t,
        pkgRevTime        : Time.time}

  fun pkgRevRepoUrl (r:pkg_revinfo) : string = #pkgRevRepoUrl r
  fun pkgRevRef (r:pkg_revinfo) : string = #pkgRevRef r
  fun pkgRevCommit (r:pkg_revinfo) : string = #pkgRevCommit r
  fun pkgRevGetManifest (r:pkg_revinfo) : Manifest.t = #pkgRevGetManifest r ()
  fun pkgRevTime (r:pkg_revinfo) : Time.time = #pkgRevTime r

  type pkg_info = {pkgVersions: (semver,pkg_revinfo)M.t,
                   pkgLookupCommit: string option -> pkg_revinfo}

  fun pkgVersions (pinfo:pkg_info) : (semver,pkg_revinfo) M.t =
      #pkgVersions pinfo

  fun lookupPkgRev (v:semver) (pi:pkg_info) : pkg_revinfo option =
      M.lookup (pkgVersions pi) v

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

  (* Shared temporary directory for all repository clones during this execution *)
  val cacheDir : string option ref = ref NONE
  
  fun getCacheDir () : string =
      case !cacheDir of
          SOME dir => dir
        | NONE =>
          let val dir = OS.FileSys.tmpName() ^ "-smlpkg-cache"
              val () = if System.doesDirExist dir then ()
                      else OS.FileSys.mkDir dir
              val () = log ("created cache directory " ^ dir)
              val () = cacheDir := SOME dir
          in dir
          end

  (* Convert repository URL to a safe directory name *)
  fun repoUrlToDir (repo_url:string) : string =
      let fun sanitize #"/" = #"-"
            | sanitize #":" = #"-"
            | sanitize #"@" = #"-"
            | sanitize #"." = #"-"
            | sanitize c = c
      in String.map sanitize repo_url
      end

  (* Clone a git repository to the cache directory and return the path.
     If the repository already exists in the cache, reuse it. *)
  fun cloneRepo (repo_url:string) : string =
      let infix </>
          val op </> = System.</>
          val cache_dir = getCacheDir()
          val repo_name = repoUrlToDir repo_url
          val repo_dir = cache_dir </> repo_name
      in if System.doesDirExist repo_dir then
           ( log ("reusing cached repository " ^ repo_dir)
           ; repo_dir )
         else
           ( log ("cloning repository " ^ repo_url ^ " to " ^ repo_dir)
           ; let val cmd = "git clone --bare " ^ System.shellEscape repo_url ^ " " ^ System.shellEscape repo_dir
                 val (status,out,err) = System.command cmd
                 val () = if OS.Process.isSuccess status then ()
                         else raise Fail ("Failed to clone " ^ repo_url ^ ": " ^ err)
             in repo_dir
             end )
      end

  (* Get the manifest from a git repository at a specific ref *)
  fun getManifestFromRepo (repo_dir:string) (refe:string) : Manifest.t =
      let val () = log ("reading manifest from " ^ repo_dir ^ " at " ^ refe)
          val manifest_file = Manifest.smlpkg_filename()
          val cmd = "git " ^ "--git-dir=" ^ System.shellEscape repo_dir ^ " show " ^ 
                   System.shellEscape (refe ^ ":" ^ manifest_file)
          val (status,out,err) = System.command cmd
          val () = if OS.Process.isSuccess status then ()
                  else raise Fail ("Failed to read " ^ manifest_file ^ " at " ^ refe ^ ": " ^ err)
          val path = repo_dir ^ "/" ^ refe ^ "/" ^ manifest_file
      in Manifest.fromString path out
      end

  (* Manifest cache to avoid reading the same manifest multiple times *)
  val cache =
      let val m : (string * Manifest.t) list ref = ref nil
      in fn f => fn repo_dir => fn refe => fn () =>
            let val s = repo_dir ^ "@" ^ refe
            in case List.find (fn (k,_) => k=s) (!m) of
                   SOME (_,v) => v
                 | NONE => let val v = f repo_dir refe
                           in m := (s,v) :: !m
                            ; v
                           end
            end
      end

  (* Create a pkg_revinfo for a specific git ref *)
  fun mkRevInfo (repo_url:string) (repo_dir:string) (refe:string) (hash:string) : pkg_revinfo =
      let val mc = cache getManifestFromRepo repo_dir refe
          val time = Time.now()
          val () = log ("rev info: " ^ repo_url ^ " @ " ^ refe ^ " (" ^ hash ^ ")")
      in {pkgRevRepoUrl=repo_url,
          pkgRevRef=refe,
          pkgRevCommit=hash,
          pkgRevGetManifest=mc,
          pkgRevTime=time}
      end

  (* Get package info from a git repository *)
  fun gitPkgInfo (repo_url:string) (versions:int list) : pkg_info =
      let val () = log ("retrieving list of tags from " ^ repo_url)
          val remote_lines = gitCmd ["ls-remote", repo_url]
          val remote_lines = String.tokens (fn c => c = #"\n") remote_lines
          
          (* Clone the repository once for reading manifests *)
          val repo_dir = cloneRepo repo_url
          
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
                                   let val pinfo = mkRevInfo repo_url repo_dir t hash
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
                     mkRevInfo repo_url repo_dir (def r) (def r)
             in {pkgVersions=rev_info,
                 pkgLookupCommit=lookupCommit}
             end
           | _ => raise Fail ("Cannot find HEAD ref for " ^ repo_url)
      end

  (* Retrieve information about a package based on its package path.
     This uses Semantic Import Versioning when interacting with
     repositories.  For example, a package @github.com/user/repo@ will
     match version 0.* or 1.* tags only, a package
     @github.com/user/repo/v2@ will match 2.* tags, and so forth..
   *)

  (* Construct repository URL from package path *)
  fun pkgpathToRepoUrl (p:pkgpath) : string =
      let val (p',_) = majorRevOfPkg p
          val base = "https://" ^ #host p ^ "/" ^ #owner p ^ "/" ^ #repo p'
      in case #host p of
             "gitlab.com" => base ^ ".git"
           | _ => base  (* Works for github.com and most other git hosts *)
      end

  (* Raw access - limited caching *)

  fun pkgInfo (p:pkgpath) : pkg_info =
      let val repo_url = pkgpathToRepoUrl p
          val (_,vs) = majorRevOfPkg p
      in gitPkgInfo repo_url vs
      end

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
                          pkgVersions = M.add (v,rev_info) (pkgVersions pinfo)}
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
                           case M.keys (pkgVersions pinfo) of
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
        in case M.keys (pkgVersions pinfo) of
               [] =>
               ( log ("package " ^ Manifest.pkgpathToString p ^
                      " has no released versions.  Using HEAD.")
               ; #1 (lookupPackageCommit p NONE))
             | v::vs =>
               let fun max (v1,v2) = if SemVer.< (v1,v2) then v2 else v1
               in log "finding newest version of packages"
                ; List.foldl max v vs  (* memo: what about versions of
                                        * equal priority - should we use
                                        * foldr? *)
               end
        end
  end
end
