structure System :> SYSTEM = struct

structure FS = OS.FileSys
structure P = OS.Process

type path = string
type filepath = path
type dirpath = path

(* Text-based file operations *)
fun readFile (f:filepath) : string =   (* may raise Fail *)
    let val is = TextIO.openIn f
    in (TextIO.inputAll is handle _ => (TextIO.closeIn is;
                                        raise Fail ("failed to read file '" ^ f ^ "'")))
       before TextIO.closeIn is
    end

fun writeFile (f:filepath) (s: string) : unit =
    let val os = TextIO.openOut f
    in (TextIO.output(os,s); TextIO.closeOut os)
       handle X => (TextIO.closeOut os; raise X)
    end

(* Binary file operations *)

fun readFileBin (f:filepath) : Word8Vector.vector =   (* may raise Fail *)
    let val is = BinIO.openIn f
    in (BinIO.inputAll is handle _ => (BinIO.closeIn is;
                                       raise Fail ("failed to read file '" ^ f ^ "'")))
       before BinIO.closeIn is
    end

fun writeFileBin (f:filepath) (s: Word8Vector.vector) : unit =
    let val os = BinIO.openOut f
    in (BinIO.output(os,s); BinIO.closeOut os)
       handle X => (BinIO.closeOut os; raise X)
    end

(* Command execution *)

(* Escape a string for safe use in shell commands *)
fun shellEscape (s: string) : string =
    "'" ^ String.translate (fn #"'" => "'\"'\"'" | c => String.str c) s ^ "'"

fun command (cmd: string) : P.status * string * string =
    let val stdoutFile = FS.tmpName()
        val stderrFile = FS.tmpName()
        fun cleanup () = (FS.remove stdoutFile; FS.remove stderrFile)
    in let val s = P.system(cmd ^ " > " ^ stdoutFile ^ " 2> " ^ stderrFile)
           val out = readFile stdoutFile
           val err = readFile stderrFile
       in (s, out, err) before cleanup()
       end handle X => (cleanup(); raise X)
    end

(* Operations on files, directories, and paths *)

fun splitPath (p:string) : string list =
    String.fields (fn c => c = #"/") p

fun hasTrailingPathSeparator (p:string) : bool =
    size p > 0 andalso String.sub(p,size p - 1) = #"/"

infix </>
fun x </> y = if x = "" then y else x ^ "/" ^ y


fun doesDirExist (p:dirpath) : bool =
    OS.FileSys.access(p,[]) andalso OS.FileSys.isDir p

fun doesFileExist (p:path) : bool =
    OS.FileSys.access(p,[]) andalso not (doesDirExist p)

fun doesLinkExist (p:path) : bool =
    OS.FileSys.access(p,[]) andalso OS.FileSys.isLink p

fun isBrokenLink (p:path): bool =
    (* Broken links will always return false on FileSys.access
       and also on FileSys.isLink.
       However, they *will* resolve to the broken link path
       with FileSys.readLink, so that's the only way I know of
       to detect them *)
    String.size(OS.FileSys.readLink p) > 0
    handle SysErr => false

fun canBeRemoved (p:path) : bool =
    (doesFileExist p orelse doesLinkExist p orelse isBrokenLink p)

fun isEmptyDir (p:dirpath) : bool =
    if doesDirExist p
    then let val stream = OS.FileSys.openDir p
             val contents = OS.FileSys.readDir stream
         in (not (Option.isSome contents)) before OS.FileSys.closeDir stream
         end
    else false

fun createDirectoryIfMissing (also_parents:bool) (p:string) : unit =
    let fun check d =
            case d of
                ".." => raise Fail "no support for '..' in dirs"
              | "." => raise Fail "no support for '.' in dirs"
              | "" => raise Fail "no support for '' in dirs"
              | _ => ()
        val dirs = splitPath p
        val () = List.app check dirs
        fun loop pre nil = ()
          | loop pre (x::xs) =
            let val d = pre </> x
            in if doesDirExist d then loop d xs
               else if null xs orelse also_parents then
                 ( (if doesFileExist d then
                      raise Fail ("cannot create directory " ^ d ^
                                  " as a file exists with that name.")
                    else OS.FileSys.mkDir d)
                 ; loop d xs)
               else raise Fail ("parent directory " ^ d ^ " of " ^ p ^ " does not exist.")
            end
    in loop "" dirs
    end

fun isRelative p =
    if size p > 0 then
      String.sub(p,0) <> #"/"
    else raise Fail "isRelative expects non-empty path"

fun isAbsolute p =
    if size p > 0 then
      String.sub(p,0) = #"/"
    else raise Fail "isAbsolute expects non-empty path"

fun makeRelative (f:dirpath) (p:path) : string =
    case (isRelative f, isRelative p) of
        (true, true) =>
        let fun loop (x::xs,y::ys) =
                if x = y then loop (xs,ys)
                else raise Fail ("makeRelative failed as " ^ f ^ " is not a subpath of " ^ p)
              | loop (nil,ys) = String.concatWith "/" ys
              | loop _ = raise Fail ("makeRelative failed as " ^ f ^ " is not a subpath of " ^ p)
        in loop(splitPath f, splitPath p)
        end
      | _ => raise Fail "makeRelative assumes relative directories as arguments"

fun getDirectoryContents (d: dirpath) : path list =
    (* returns fully-prefixed contents, like POSIX `find`, NOT like POSIX `ls` *)
    let val stream = OS.FileSys.openDir d
        fun getAll acc = case OS.FileSys.readDir stream of
                             NONE => rev acc
                           | SOME entry => getAll ((d </> entry)::acc)
    in getAll [] before OS.FileSys.closeDir stream
    end

fun removePathForcibly (p:path) : unit =
    if canBeRemoved p then OS.FileSys.remove p
    else if isEmptyDir p then OS.FileSys.rmDir p
    else if doesDirExist p then (app removePathForcibly (getDirectoryContents p)
                                ; removePathForcibly p )
    else ()

fun renameDirectory (old:dirpath) (new:dirpath) : unit =
    if doesFileExist new then
      raise Fail ("cannot rename directory as target directory '" ^
                  new ^ "' already exists")
    else if doesDirExist old then
      OS.FileSys.rename{old=old,new=new}
    else raise Fail ("'" ^ old ^ "' is not a directory.")

fun takeDirectory (p:path) : dirpath = OS.Path.dir p

end
