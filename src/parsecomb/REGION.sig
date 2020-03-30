signature REGION = sig
  type filename = string
  type loc = int * int * filename
  type reg = loc * loc

  val botloc  : loc
  val loc0    : filename -> loc (* line 1, char 1 *)
  val newline : loc -> loc
  val next    : loc -> loc
  val lt      : loc -> loc -> bool
  val wf      : reg -> bool                      
  val ppLoc   : loc -> string      
  val pp      : reg -> string
  val plus    : string -> reg -> reg -> reg
end


(* 

botloc
    end of file

loc0 f
    line 1, char 1 of file f

newline
    first char on next line

next
    next char on current line

lt a b
    is location a strictly before location b in the file

wf r
    well formed

ppLoc
    pretty print location

pp
    pretty print region

plus
    merge regions (string only used for error reporting)

*)
