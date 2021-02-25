open! Core
open Lambda_calc

let () = try (
  while true do
    print_string "Enter a Lambda Calculus expression > ";
    Out_channel.(flush stdout);
    let s = In_channel.(input_line_exn stdin) in
    let lam = Util.parse s in
    match lam with
    | Error e -> print_s [%message (e: Error.t)]
    | Ok lam ->
      print_string "Parsed: ";
      Lambda.print lam;
      print_endline "Attempting up to 1000 NOR reductions";
      let reduced = Lambda.nor lam 1000 in
      print_string "Reduced: ";
      Lambda.print reduced;
      print_string "Reduced (DeBruijn): ";
      reduced |> Lambda.to_debruijn |> Lambda.print
  done
) with End_of_file -> ()
;;
