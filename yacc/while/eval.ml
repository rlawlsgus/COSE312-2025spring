open Ast

module State = struct
  type t = (var * int) list
  let empty = []
  let rec lookup s x =
  match s with
  | [] -> raise (Failure (x ^ " is not bound in state"))
  | (y,v)::s' -> if x = y then v else lookup s' x
  let update s x v = (x,v)::s
end 

let rec eval_a : aexp -> State.t -> int
=fun a s ->
  match a with
  | Int n -> n
  | Var x -> State.lookup s x
  | Add (a1, a2) -> (eval_a a1 s) + (eval_a a2 s)
  | Sub (a1, a2) -> (eval_a a1 s) - (eval_a a2 s)
  | Mul (a1, a2) -> (eval_a a1 s) * (eval_a a2 s)
  | Div (a1, a2) -> (eval_a a1 s) / (eval_a a2 s)
  | Mod (a1, a2) -> (eval_a a1 s) mod (eval_a a2 s)

let rec eval_b : bexp -> State.t -> bool
=fun b s ->
  match b with
  | Bool true -> true
  | Bool false -> false
  | Eq (a1, a2) -> (eval_a a1 s) = (eval_a a2 s)
  | Le (a1, a2) -> (eval_a a1 s) <= (eval_a a2 s)
  | Neg b' -> not (eval_b b' s)
  | Conj (b1, b2) -> (eval_b b1 s) && (eval_b b2 s)  

let rec eval_c : cmd -> State.t -> State.t
=fun c s ->
  match c with
  | Assign (x, a) -> State.update s x (eval_a a s)
  | Skip -> s
  | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
  | If (b, c1, c2) -> eval_c (if eval_b b s then c1 else c2) s
  | While (b, c) ->
    if eval_b b s then eval_c (While (b,c)) (eval_c c s)
    else s
  | Print a -> print_endline (string_of_int (eval_a a s)); s 

let eval : program -> State.t 
=fun p -> eval_c p State.empty 