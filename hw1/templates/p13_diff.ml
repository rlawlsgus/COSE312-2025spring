exception NotImplemented;;

type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  | Const n -> Const 0
  | Var y -> if y = x then Const 1 else Const 0
  | Power (y, n) -> if y = x then Times [Const n; Power (y, n-1)] else Const 0
  | Times lst -> (match lst with
    | Const 0::tl -> Const 0
    | hd::Const 0::tl -> Const 0
    | [] -> Const 0
    | hd::[] -> diff (hd, x)
    | hd::tl -> Sum [Times (diff (hd, x)::tl); Times [hd; diff (Times tl, x)]])
  | Sum lst -> (match lst with
    | Const 0::tl -> diff (Sum tl, x)
    | hd::Const 0::tl -> diff (Sum tl, x)
    | [] -> Const 0
    | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)]);;
