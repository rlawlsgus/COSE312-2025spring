exception NotImplemented;;

type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;


let rec calculator : exp -> int
= fun exp -> 
  let rec eval : (exp * int) -> int
  = fun (exp, x) -> match exp with
    | X -> x
    | INT n -> n
    | ADD (e1, e2) -> eval (e1, x) + eval (e2, x)
    | SUB (e1, e2) -> eval (e1, x) - eval (e2, x)
    | MUL (e1, e2) -> eval (e1, x) * eval (e2, x)
    | DIV (e1, e2) -> eval (e1, x) / eval (e2, x)
    | SIGMA (e1, e2, e3) -> 
      let rec sigma (a, b, f) =
        if a > b then 0
        else eval (f, a) + sigma (a+1, b, f) in
      sigma (eval (e1, x), eval (e2, x), e3)
      in match exp with
  | X -> raise NotImplemented
  | INT n -> n
  | ADD (e1, e2) -> calculator e1 + calculator e2
  | SUB (e1, e2) -> calculator e1 - calculator e2
  | MUL (e1, e2) -> calculator e1 * calculator e2
  | DIV (e1, e2) -> calculator e1 / calculator e2
  | SIGMA (e1, e2, e3) -> 
    let rec sigma (a, b, f) =
      if a > b then 0
      else eval (f, a) + sigma (a+1, b, f) in
    sigma (calculator e1, calculator e2, e3);;
