
signature SYSTEM = sig

  type path = string
  type filepath = path
  type dirpath = path

  val readFile                 : filepath -> string
  val writeFile                : filepath -> string -> unit

  val readFileBin              : filepath -> Word8Vector.vector
  val writeFileBin             : filepath -> Word8Vector.vector -> unit

  val command                  : string -> OS.Process.status * string * string

  val splitPath                : path -> string list
  val createDirectoryIfMissing : bool -> dirpath -> unit
  val hasTrailingPathSeparator : path -> bool

  val makeRelative             : dirpath -> path -> string   (* for relative paths only *)

  val doesFileExist            : path -> bool                (* also returns true if path is a directory *)
  val doesDirExist             : dirpath -> bool

  val removePathForcibly       : path -> unit

  val </>                      : dirpath * path -> path
end
