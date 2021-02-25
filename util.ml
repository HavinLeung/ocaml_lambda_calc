open! Core
open Lexing

let parse s =
  let lexbuf = Lexing.from_string s in
  (try Parser.prog Lexer.read lexbuf |> Or_error.return with
  | _ ->
    let position = lexbuf.lex_curr_pos in
    Or_error.error_s [%message "Error at" (position : int)])
  |> Or_error.map ~f:(fun expr -> Lambda.create expr)
;;

let parse_exn s = parse s |> Or_error.ok_exn

let parse_reduce_print
    (s : string)
    (f : 'a Lambda.t -> int -> 'a Lambda.t)
    (steps : int) : unit Or_error.t =
  parse s |> Or_error.map ~f:(fun l ->
      print_endline "Parsed:";
      Lambda.print l;
      let l = f l steps in
      print_endline "Reduced:";
      Lambda.print l;
      print_endline "DeBruijn:";
      Lambda.to_debruijn l |> Lambda.print;
    )
;;
