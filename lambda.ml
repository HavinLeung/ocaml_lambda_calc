open! Core

type t = {
  expr : Lambda_expr.t;
  is_debruijn : bool
}

let create (expr: Lambda_expr.t) = {expr;is_debruijn=false}

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf
;;

let print {expr;_} = Lambda_expr.print expr

let to_debruijn ({expr; is_debruijn} : t) = 
  if is_debruijn then raise_s [%message "already debruijn"];
  let rec to_debruijn (expr: Lambda_expr.t) (map : int String.Map.t) = 
    match expr with
    | `Variable x ->
      (match Map.find map x with
       | None -> expr
       | Some v -> `Variable (Int.to_string v))
    | `Abstraction (x,m) ->
      let new_map = Map.map map ~f:(fun i -> i+1) in
      let new_map = Map.update new_map x ~f:(fun _ -> 1) in
      `Abstraction ("", to_debruijn m new_map)
    | `Application (n,m) ->
      `Application ((to_debruijn n map), (to_debruijn m map))
  in
  {expr=(to_debruijn expr String.Map.empty); is_debruijn=true}
;;

let reduce t (steps: int) f : t =
  if t.is_debruijn then raise_s [%message "called aoe after to_debruijn"];
  let rec repeat t steps =
    if steps <= 0 then t
    else (match f t with
        | None -> t
        | Some t -> repeat t (steps - 1)
      )
  in
  repeat t steps
;;

let aoe {expr; is_debruijn} : t option =
  if is_debruijn then raise_s [%message "called aoe after to_debruijn"];
  let rec aoe :(Lambda_expr.t -> Lambda_expr.t option) = function
    | `Variable _ | `Abstraction _ -> None
    | `Application (rator, rand) ->
      (match rator with
       | `Abstraction (var, body) ->
         (match aoe rand with
          | Some rand -> Some (`Application (rator, rand))
          | None -> Lambda_expr.substitute body var rand |> Option.return)
       | rator ->
         (match aoe rator with
          | Some rator -> `Application (rator, rand) |> Option.return
          | None -> Option.map (aoe rand) ~f:(fun rand -> `Application (rator, rand)))
      )
  in
  Option.map (aoe expr) ~f:(fun expr -> {expr; is_debruijn})
;;

let aoe t (steps: int) : t =
  reduce t steps aoe
;;

let nor {expr; is_debruijn} : t option =
  if is_debruijn then raise_s [%message "called nor after to_debruijn"];
  let rec nor : (Lambda_expr.t -> Lambda_expr.t option) = function
    | `Variable _ -> None
    | `Abstraction (x, m) -> Option.map (nor m) ~f:(fun m' -> `Abstraction (x, m'))
    | `Application (`Abstraction (x, m1), m2) -> Lambda_expr.substitute m1 x m2 |> Option.return
    | `Application (m1, m2) ->
      (match nor m1 with
       | Some m1' -> `Application (m1', m2) |> Option.return
       | None -> Option.map (nor m2) ~f:(fun m2' -> `Application (m1, m2'))
      )
  in
  Option.map (nor expr) ~f:(fun expr -> {expr; is_debruijn})
;;

let nor t steps = reduce t steps nor

let eta {expr; is_debruijn} : t option =
  if is_debruijn then raise_s [%message "called eta after to_debruijn"];
  let rec eta : (Lambda_expr.t -> Lambda_expr.t option) = function
    | `Variable _ -> None
    | `Abstraction (x, `Application (m',`Variable y)) when y = x && not (Set.mem (Lambda_expr.freevars m') x ) -> Some m'
    | `Abstraction (x, m) -> Option.map (eta m) ~f:(fun m' -> `Abstraction (x, m'))
    | `Application (m1,m2) ->
      (match eta m1 with
       | Some m1' -> `Application (m1', m2) |> Option.return
       | None -> Option.map (eta m2) ~f:(fun m2' -> `Application (m1, m2'))
      )
  in
  Option.map (eta expr) ~f:(fun expr -> {expr; is_debruijn})
;;

let eta t steps = reduce t steps eta
