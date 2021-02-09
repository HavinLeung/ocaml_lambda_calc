open Core
open Lexer
open Lexing

let raise_position lexbuf =
  let {pos_fname; pos_lnum; pos_cnum; pos_bol;_} = lexbuf.lex_curr_p in
  raise_s [%message (pos_fname : string) (pos_lnum : int) (pos_cnum : int) (pos_bol : int)]

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError _ ->
    raise_position lexbuf
  | Parser.Error ->
    raise_position lexbuf

let parse s =
  let lexbuf = Lexing.from_string s in
  parse_with_error lexbuf
;;

let%test_module _ = (module struct
  let exprs = [
    {|\x.(\y.x)x|};
    {|\x.\y.x|};
    {|\n.(\n.(\n.(\n.n)n)n)n|};
    {|(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))|};
    {|(\head.\tail.\cons.\zero.\two.(\succ.(\pred.(pred two)) (\n.(tail(n(\p.cons(succ(head p)) (head p))(cons zero zero))))) (\n.\f.\x.n f(f x))) (\l.l\x.\y.x) (\l.l\x.\y.y) (\h.\t.\s.s h t) (\f.\x.x) (\f.\x.f (f x)) f x|};
    {|\m.\n.\f.\x.n m f x|};
    {|(\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f x|};
  ]
  ;;

  let%expect_test "parse and print" =
    List.iter exprs ~f:(fun expr ->
        print_endline ("parsing: " ^ expr);
        let expr = parse expr in
        Lambda_expr.print expr;
        Lambda.create expr |> Lambda.to_debruijn |> Lambda.print;
        print_endline "";
      );
    [%expect{|
      parsing: \x.(\y.x)x
      (\x.((\y.x) x))
      (\.((\.2) 1))

      parsing: \x.\y.x
      (\x.(\y.x))
      (\.(\.2))

      parsing: \n.(\n.(\n.(\n.n)n)n)n
      (\n.((\n.((\n.((\n.n) n)) n)) n))
      (\.((\.((\.((\.1) 1)) 1)) 1))

      parsing: (\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))
      (((\mul.(\two.((mul two) two))) (\m.(\n.(\f.(m (n f)))))) (\f.(\x.(f (f x)))))
      (((\.(\.((2 1) 1))) (\.(\.(\.(3 (2 1)))))) (\.(\.(2 (2 1)))))

      parsing: (\head.\tail.\cons.\zero.\two.(\succ.(\pred.(pred two)) (\n.(tail(n(\p.cons(succ(head p)) (head p))(cons zero zero))))) (\n.\f.\x.n f(f x))) (\l.l\x.\y.x) (\l.l\x.\y.y) (\h.\t.\s.s h t) (\f.\x.x) (\f.\x.f (f x)) f x
      ((((((((\head.(\tail.(\cons.(\zero.(\two.((\succ.((\pred.(pred two)) (\n.(tail ((n (\p.((cons (succ (head p))) (head p)))) ((cons zero) zero)))))) (\n.(\f.(\x.((n f) (f x))))))))))) (\l.(l (\x.(\y.x))))) (\l.(l (\x.(\y.y))))) (\h.(\t.(\s.((s h) t))))) (\f.(\x.x))) (\f.(\x.(f (f x))))) f) x)
      ((((((((\.(\.(\.(\.(\.((\.((\.(1 3)) (\.(6 ((1 (\.((6 (3 (8 1))) (8 1)))) ((5 4) 4)))))) (\.(\.(\.((3 2) (2 1))))))))))) (\.(1 (\.(\.2))))) (\.(1 (\.(\.1))))) (\.(\.(\.((1 3) 2))))) (\.(\.1))) (\.(\.(2 (2 1))))) f) x)

      parsing: \m.\n.\f.\x.n m f x
      (\m.(\n.(\f.(\x.(((n m) f) x)))))
      (\.(\.(\.(\.(((3 4) 2) 1)))))

      parsing: (\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f x
      (((\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f) x)
      (((\.((\.(\.(2 (2 1)))) ((\.(\.(2 (2 1)))) 1))) f) x) |}]
  ;;

  let reduce_and_test exprs reduce steps =
    List.iter exprs ~f:(fun expr ->
        print_endline expr;
        let expr = parse expr |> Lambda.create in
        let expr = reduce expr steps in
        Lambda.print expr;
        Lambda.to_debruijn expr |> Lambda.print;
        print_endline "";
      )
  ;;

  let%expect_test "aoe" =
    reduce_and_test exprs Lambda.aoe 1000;
    [%expect {|
      \x.(\y.x)x
      (\x.((\y.x) x))
      (\.((\.2) 1))

      \x.\y.x
      (\x.(\y.x))
      (\.(\.2))

      \n.(\n.(\n.(\n.n)n)n)n
      (\n.((\n.((\n.((\n.n) n)) n)) n))
      (\.((\.((\.((\.1) 1)) 1)) 1))

      (\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))
      (\f!!!!!!!.((\f!!.(\x!!!!.(f!! (f!! x!!!!)))) ((\f.(\x.(f (f x)))) f!!!!!!!)))
      (\.((\.(\.(2 (2 1)))) ((\.(\.(2 (2 1)))) 1)))

      (\head.\tail.\cons.\zero.\two.(\succ.(\pred.(pred two)) (\n.(tail(n(\p.cons(succ(head p)) (head p))(cons zero zero))))) (\n.\f.\x.n f(f x))) (\l.l\x.\y.x) (\l.l\x.\y.y) (\h.\t.\s.s h t) (\f.\x.x) (\f.\x.f (f x)) f x
      (f x)
      (f x)

      \m.\n.\f.\x.n m f x
      (\m.(\n.(\f.(\x.(((n m) f) x)))))
      (\.(\.(\.(\.(((3 4) 2) 1)))))

      (\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f x
      (f (f (f (f x))))
      (f (f (f (f x)))) |}]
  ;;

  let%expect_test "nor" =
    let exprs = {|(\head.\tail.\cons.\isNull.\nil.\zero.\succ.(\Y.\F.(\len.(len (cons zero (cons zero nil)) )) (Y F)) (\f.(\x.f(x x))(\x.f(x x))) (\f.\l.(isNull l) zero (succ (f(tail l))))) (\l.l(\x.\y.x)) (\l.l(\x.\y.y)) (\h.\t.\s.s h t) (\l.l\h.\t.\x.\y.y) (\s.\x.\y.x) (\f.\x.x) (\n.\f.\x.n f(f x))|} :: exprs in
    reduce_and_test exprs Lambda.nor 1000;
    [%expect {|
      (\head.\tail.\cons.\isNull.\nil.\zero.\succ.(\Y.\F.(\len.(len (cons zero (cons zero nil)) )) (Y F)) (\f.(\x.f(x x))(\x.f(x x))) (\f.\l.(isNull l) zero (succ (f(tail l))))) (\l.l(\x.\y.x)) (\l.l(\x.\y.y)) (\h.\t.\s.s h t) (\l.l\h.\t.\x.\y.y) (\s.\x.\y.x) (\f.\x.x) (\n.\f.\x.n f(f x))
      (\f!!!!!!!!!!!!!!!.(\x!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.(f!!!!!!!!!!!!!!! (f!!!!!!!!!!!!!!! x!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!))))
      (\.(\.(2 (2 1))))

      \x.(\y.x)x
      (\x.x)
      (\.1)

      \x.\y.x
      (\x.(\y.x))
      (\.(\.2))

      \n.(\n.(\n.(\n.n)n)n)n
      (\n.n)
      (\.1)

      (\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))
      (\f!!!!!!!.(\x!!!!!.(f!!!!!!! (f!!!!!!! (f!!!!!!! (f!!!!!!! x!!!!!))))))
      (\.(\.(2 (2 (2 (2 1))))))

      (\head.\tail.\cons.\zero.\two.(\succ.(\pred.(pred two)) (\n.(tail(n(\p.cons(succ(head p)) (head p))(cons zero zero))))) (\n.\f.\x.n f(f x))) (\l.l\x.\y.x) (\l.l\x.\y.y) (\h.\t.\s.s h t) (\f.\x.x) (\f.\x.f (f x)) f x
      (f x)
      (f x)

      \m.\n.\f.\x.n m f x
      (\m.(\n.(\f.(\x.(((n m) f) x)))))
      (\.(\.(\.(\.(((3 4) 2) 1)))))

      (\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f x
      (f (f (f (f x))))
      (f (f (f (f x)))) |}]
  ;;

  let%expect_test "eta" =
    let exprs = [
      {|\m.\n.\f.\x.m(n f)x|};
      {|\m.\n.\f.\x.n m f x|};
      {|\x.(\x.(\x.(\y.y)x)x)x|};
      {|\a.(a b c d \.foo foo bar)a|};
    ] @ exprs in
    reduce_and_test exprs Lambda.eta 1000;
    [%expect {|
      \m.\n.\f.\x.m(n f)x
      (\m.(\n.(\f.(m (n f)))))
      (\.(\.(\.(3 (2 1)))))

      \m.\n.\f.\x.n m f x
      (\m.(\n.(n m)))
      (\.(\.(1 2)))

      \x.(\x.(\x.(\y.y)x)x)x
      (\y.y)
      (\.1)

      \a.(a b c d \.foo foo bar)a
      (\a.(((((a b) c) d) (\.((foo foo) bar))) a))
      (\.(((((1 b) c) d) (\.((foo foo) bar))) 1))

      \x.(\y.x)x
      (\x.((\y.x) x))
      (\.((\.2) 1))

      \x.\y.x
      (\x.(\y.x))
      (\.(\.2))

      \n.(\n.(\n.(\n.n)n)n)n
      (\n.n)
      (\.1)

      (\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))
      (((\mul.(\two.((mul two) two))) (\m.(\n.(\f.(m (n f)))))) (\f.(\x.(f (f x)))))
      (((\.(\.((2 1) 1))) (\.(\.(\.(3 (2 1)))))) (\.(\.(2 (2 1)))))

      (\head.\tail.\cons.\zero.\two.(\succ.(\pred.(pred two)) (\n.(tail(n(\p.cons(succ(head p)) (head p))(cons zero zero))))) (\n.\f.\x.n f(f x))) (\l.l\x.\y.x) (\l.l\x.\y.y) (\h.\t.\s.s h t) (\f.\x.x) (\f.\x.f (f x)) f x
      ((((((((\head.(\tail.(\cons.(\zero.(\two.((\succ.((\pred.(pred two)) (\n.(tail ((n (\p.((cons (succ (head p))) (head p)))) ((cons zero) zero)))))) (\n.(\f.(\x.((n f) (f x))))))))))) (\l.(l (\x.(\y.x))))) (\l.(l (\x.(\y.y))))) (\h.(\t.(\s.((s h) t))))) (\f.(\x.x))) (\f.(\x.(f (f x))))) f) x)
      ((((((((\.(\.(\.(\.(\.((\.((\.(1 3)) (\.(6 ((1 (\.((6 (3 (8 1))) (8 1)))) ((5 4) 4)))))) (\.(\.(\.((3 2) (2 1))))))))))) (\.(1 (\.(\.2))))) (\.(1 (\.(\.1))))) (\.(\.(\.((1 3) 2))))) (\.(\.1))) (\.(\.(2 (2 1))))) f) x)

      \m.\n.\f.\x.n m f x
      (\m.(\n.(n m)))
      (\.(\.(1 2)))

      (\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f x
      (((\f.((\f.(\x.(f (f x)))) ((\f.(\x.(f (f x)))) f))) f) x)
      (((\.((\.(\.(2 (2 1)))) ((\.(\.(2 (2 1)))) 1))) f) x) |}]
  ;;
end)
