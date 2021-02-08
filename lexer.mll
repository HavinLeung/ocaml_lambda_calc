{
open Parser

exception SyntaxError of string
}

let var = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let white = [' ' '\t']+

rule read = 
  parse 
  | white     {read lexbuf}
  | var as v  {VAR v}
  | "\\"      {LAMBDA}
  | "("       {LPAREN}
  | ")"       {RPAREN}
  | "."       {DOT}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
  | eof {EOF}