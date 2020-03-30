structure Lex :> LEX =
struct
structure R = Region
type reg = R.reg
type loc = R.loc
type filename = R.filename
datatype token = Symb of char
               | Id of string

fun pr_token (Symb c) = "'" ^ String.str c ^ "'"
  | pr_token (Id s) = "'" ^ s ^ "'"

fun loc0 (f:filename) : loc = (1,0,f) (* line 1, char 0 *)

datatype state = StartS
               | IdS of string * loc * loc

fun isSymb c = CharVector.exists (fn c' => c'=c) "{}/"

fun lexError (loc:loc) (s:string) : 'a =
    let val msg = "Lexical error at location " ^ R.ppLoc loc ^ ": " ^ s
    in raise Fail msg
    end

type lexstate = (token * reg) list * state * loc

fun process (c:char,(tokens,state,loc):lexstate) : lexstate =
    let fun next' c loc = if c = #"\n" then R.newline loc else R.next loc
        fun proc (tokens,state,loc) =
            case state of
                StartS =>
                if Char.isSpace c then (tokens, state, next' c loc)
                else if isSymb c then ((Symb c,(loc,loc))::tokens, StartS, R.next loc)
                else if Char.isPrint c then (tokens, IdS(String.str c,loc,loc), R.next loc)
                else lexError loc "expecting printable character"
              | IdS(s,l0,l1) =>
                if Char.isSpace c then ((Id s,(l0,l1))::tokens, StartS, next' c loc)
                else if isSymb c then ((Symb c,(loc,loc))::(Id s,(l0,l1))::tokens, StartS, R.next loc)
                else if Char.isPrint c then (tokens, IdS(s ^ String.str c,l0,loc), R.next loc)
                else lexError loc "expecting printable character"
    in proc(tokens,state,loc)
    end

fun lex (filename: filename) (s:string) : (token * reg) list =
    let val s = s^" " (* pad some whitespace to keep the lexer happy *)
        val (tokens,state,_) = CharVector.foldl process (nil,StartS,R.loc0 filename) s
    in rev tokens
    end

end
