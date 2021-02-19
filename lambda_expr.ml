open! Core

type t = [
  | `Variable of string
  | `Abstraction of (string * t)
  | `Application of (t * t)
] [@@deriving sexp, equal]

let print (t:t) = 
  let rec print (t:t) = match t with
    | `Variable v -> print_string (v)
    | `Abstraction (v, t) ->
      print_string ("(\\" ^ v ^ ".");
      print t;
      print_string ")";
    | `Application (m,n) ->
      print_string "(";
      print m;
      print_string " ";
      print n;
      print_string ")";
  in
print t;
print_endline "";
;;

let rec freevars (t:t) = match t with
  | `Variable x -> String.Set.of_list [x]
  | `Abstraction (x,t) -> 
    let fv = freevars t in
    String.Set.remove fv x
  | `Application (m,n) ->
    String.Set.union (freevars m) (freevars n)
;;

let rec substitute (t:t) (x:string) (sub:t) : t =
  match t with
  | `Variable mx as self ->
    if String.equal mx x then sub else self
  | `Application (rator, rand) ->
    `Application ((substitute rator x sub), (substitute rand x sub))
  | `Abstraction (var, body) as self ->
    (match var with
    | _ when x = var -> self
    | var -> (
      match Set.mem (freevars sub) var with
      | false -> `Abstraction (var, substitute body x sub)
      | true ->
        let z = ref var in
        while Set.mem (freevars sub) !z do
          z := (!z ^ "!");
        done;
        let new_body = substitute body var (`Variable !z) in
        `Abstraction (!z, substitute new_body x sub)
      )
    )
;;
