(* Semantic versioning; see https://semver.org/ *)

structure SemVer :> SEMVER = struct

datatype id = NUMID of IntInf.int | ALPHAID of string

type t = {major  : int,
          minor  : int,
          patch  : int,
          prerel : id list,
          build  : id list}

fun idToString (NUMID i) = IntInf.toString i
  | idToString (ALPHAID s) = s

fun idsToString ids =
    String.concatWith "." (map idToString ids)

fun toString ({major,minor,patch,prerel,build}:t) : string =
    Int.toString major ^ "." ^
    Int.toString minor ^ "." ^
    Int.toString patch ^
    (case prerel of
         nil => ""
       | ids => "-" ^ idsToString ids) ^
    (case build of
         nil => ""
       | ids => "+" ^ idsToString ids)

fun fromString (s: string) : t option =
    let fun parseAlphaId s : id =
            if String.size s > 0 andalso
               CharVector.all (fn c => c = #"-" orelse Char.isDigit c orelse Char.isLower c
                                       orelse Char.isUpper c) s
            then ALPHAID s
            else raise Fail ("expecting alpha-id - got " ^ s)
        fun parseId s : id =
            case IntInf.fromString s of
                SOME n => if n > 0 andalso IntInf.toString n = s then NUMID n
                          else parseAlphaId s
              | NONE => parseAlphaId s
        fun parseIds (s:string) : id list =
            if s = "" then nil
            else map parseId (String.fields (fn c => c = #".") s)
        val (prebuild : string, build : id list) =
            case String.fields (fn c => c = #"+") s of
                [a,b] => (a,parseIds b)
              | [a] => (a,nil)
              | _ => raise Fail "expecting at most one +"
        val (pre : string, prerel : id list) =
            case String.fields (fn c => c = #"-") prebuild of
                pre::rest => (pre,parseIds (String.concatWith "-" rest))
              | _ => raise Fail "impossible"
        val (major,minor,patch) =
            case map Int.fromString (String.fields (fn c => c = #".") pre) of
                [SOME ma,SOME mi,SOME pa] =>
                if ma >= 0 andalso mi >= 0 andalso pa >= 0 andalso
                   Int.toString ma ^ "." ^ Int.toString mi ^ "." ^ Int.toString pa = pre
                then (ma,mi,pa)
                else raise Fail "misformed major.minor.patch specification"
              | _ => raise Fail "too many dots"
    in SOME {major=major,
             minor=minor,
             patch=patch,
             prerel=prerel,
             build=build}
    end handle Fail _ => NONE

fun major (t:t) : int = #major t
fun minor (t:t) : int = #minor t
fun patch (t:t) : int = #patch t
fun prerel (t:t) : id list = #prerel t
fun build  (t:t) : id list = #build t

fun idLt (id1:id, id2:id) : bool =
    case (id1, id2) of
         (NUMID id1, NUMID id2) => id1 < id2
       | (NUMID _, _) => true
       | (_, NUMID _) => false
       | (ALPHAID s1, ALPHAID s2) => s1 < s2

fun idsLt (nil, _) = false
  | idsLt (_, nil) = true
  | idsLt (ids1: id list, ids2: id list) : bool =
    let fun lt (ids1,ids2) =
            case (ids1,ids2) of
                (nil, nil) => false
              | (nil, _) => true
              | (id1::ids1,id2::ids2) =>
                idLt(id1,id2) orelse
                (id1 = id2 andalso lt(ids1,ids2))
              | _ => false
    in lt (ids1,ids2)
    end

val op < : t * t -> bool =
 fn (t1,t2) =>
    major t1 < major t2 orelse
    (major t1 = major t2 andalso
     (minor t1 < minor t2 orelse
      (minor t1 = minor t2 andalso
       (patch t1 < patch t2 orelse
        (patch t1 = patch t2 andalso
         (idsLt(prerel t1, prerel t2)))))))
end
