%{
open Ast
%}

%token <string> ID
%token LAMBDA DOT
%token QUOTE
%token LET BE
%token TO
%token PRODUCE THUNK FORCE
%token LPAREN RPAREN
%token EOF

%start <Ast.exp> prog

%%

prog:
  | exp; EOF                      { $1               }
  ;

exp:
  | ID                            { Var $1           }
  | LAMBDA; ID; DOT; exp          { Fun ($2, $4)     }
  | exp; QUOTE; exp               { App ($1, $3)     }
  | LET; ID; BE; exp; DOT; exp    { Let ($2, $4, $6) }
  | exp; TO; ID; DOT; exp         { To ($1, $3, $5)  }
  | PRODUCE; exp                  { Produce ($2)     }
  | THUNK; exp                    { Thunk ($2)       }
  | FORCE; exp                    { Force ($2)       }
  | LPAREN; exp; RPAREN           { $2               }
  ;