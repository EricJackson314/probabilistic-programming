{
open Parser
}

let white = [' ' '\t' '\n'] *
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white     { read lexbuf               }
  | "fun"     { LAMBDA                    }
  | "."       { DOT                       }
  | "'"       { QUOTE                     }
  | "let"     { LET                       }
  | "be"      { BE                        }
  | "to"      { TO                        }
  | "produce" { PRODUCE                   }
  | "thunk"   { THUNK                     }
  | "force"   { FORCE                     }
  | "("       { LPAREN                    }
  | ")"       { RPAREN                    }
  | id        { ID (Lexing.lexeme lexbuf) }
  | eof       { EOF                       }