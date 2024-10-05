signature ZIP = sig
  type t            (* type of archive *)
  val fromFile      : string -> t
  val download      : string -> t
  val extractSubDir : {log:string->unit} -> t -> {path:string,target:string} -> unit
  val delete        : t -> unit
end

structure Zip :> ZIP = struct

type t = {zipfile:string,deleted:bool ref}

fun fromFile (s:string) : t =
    {zipfile=s,deleted=ref false}

fun download (url:string) : t =
    let val localzip = OS.FileSys.tmpName()
        val (status,out,err) = System.command ("curl -L -o " ^ localzip ^ " " ^ url)
    in if OS.Process.isSuccess status then
         {zipfile=localzip,deleted=ref false}
       else raise Fail ("failed to download " ^ url ^ ": " ^ err)
    end

fun delete ({zipfile,deleted}:t) : unit =
    if !deleted then ()
    else ( OS.FileSys.remove zipfile
         ; deleted := true )
         handle _ => raise Fail ("failed to delete " ^ zipfile)

fun extractSubDir {log: string -> unit} ({zipfile,deleted}:t) {path:string,target:string} : unit =
    if !deleted then raise Fail ("extractSubDir: " ^ zipfile ^
                                 " has been deleted.")
    else
      (* memo: check that zipfile does not contain absolute paths, that path is
         not absolute and that no paths involve '..' *)
      let val () = log ("creating directory " ^ target)
          val () = System.createDirectoryIfMissing true target
          fun cmds zipfile path target : {zipcmd:string,mvcmd:string,tmpdir:string} =
              let val tmpdir = target ^ "_tmp~"
              in {zipcmd = "unzip " ^ zipfile ^ " '" ^ path ^ "/**' -d " ^ tmpdir,
                  mvcmd = "mv " ^ tmpdir ^ "/" ^ path ^ "/* " ^ target ^ "/",
                  tmpdir = tmpdir}
              end
          val {zipcmd, mvcmd, tmpdir} = cmds zipfile path target
          (* execute commands *)

          val () = log ("removing " ^ tmpdir)
          val () = System.removePathForcibly tmpdir

          val () = log ("cmd: " ^ zipcmd)
          val (status,out,err) = System.command zipcmd

          val () = log ("removing " ^ zipfile)
          val () = System.removePathForcibly zipfile

          val () = if OS.Process.isSuccess status then ()
                   else raise Fail ("failed to extract " ^ zipfile ^ ": " ^ err)

          val () = log ("cmd: " ^ mvcmd)
          val (status,out,err) = System.command mvcmd

          val () = log ("removing " ^ tmpdir)
          val () = System.removePathForcibly tmpdir

          val () = if OS.Process.isSuccess status then ()
                   else raise Fail ("failed to move tmpdir into target: " ^ err)
      in ()
      end

end
