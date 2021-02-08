// mostly taken from github.com/wenyuzhao/Lambda/blob/master/src/parser.mly
%token LPAREN
%token RPAREN
%token <string> VAR
%token DOT
%token LAMBDA
%token EOF
%right LPAREN VAR DOT LAMBDA 

%start <Lambda_expr.t> prog
%%

prog:
  | e = expr; EOF   {e};

expr:
  | a = atom      {a}
  | e = expr; a = atom {`Application (e,a)};

atom:
  | LPAREN; e = expr; RPAREN        {e}
  | v = VAR                         {`Variable v}
  | LAMBDA; v = VAR; DOT; e = expr  {`Abstraction (v,e)}
