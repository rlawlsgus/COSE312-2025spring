open Ast 

let rec eval : expr -> int
=fun e ->
  match e with
  | Num n -> n
  | Add (e1, e2) -> (eval e1) + (eval e2)
  | Sub (e1, e2) -> (eval e1) - (eval e2)
  | Mul (e1, e2) -> (eval e1) * (eval e2)
  | Div (e1, e2) -> (eval e1) / (eval e2)
  | Pow (e1, e2) -> pow (eval e1) (eval e2)

and pow a b = 
  if b = 0 then 1 else a * pow a (b-1) 