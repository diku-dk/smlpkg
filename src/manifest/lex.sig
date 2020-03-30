signature LEX = sig
  type reg = Region.reg
  type loc = Region.loc
  type filename = string

  datatype token = Symb of char
                 | Id of string

  val pr_token : token -> string

  val lex : filename -> string -> (token * reg) list
end
