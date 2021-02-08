%token LPAREN
%token RPAREN
%token <string> VAR
%token DOT
%token LAMBDA
%token EOF

%start <Lambda_expr.t> prog
%%

prog:
  | e = expr; EOF   {e};

expr:
  | x = atom    { x }
  | x = abs     { x }
  | x = app     { x }

abs:
  | LAMBDA; v = VAR; DOT; e = expr  { `Abstraction (v, e) }

app:
  | m = simple_app; n = abs      { `Application (m, n) }
  | m = simple_app; n = atom     { `Application (m, n) }

simple_app:
  | a = atom                  { a }
  | a = simple_app; b = atom  {`Application (a, b)}

atom:
  | LPAREN; e = expr; RPAREN  { e }
  | v = VAR                   { `Variable v }
