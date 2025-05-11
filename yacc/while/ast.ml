type var = string

type aexp =
| Int of int
| Var of var
| Add of aexp * aexp
| Sub of aexp * aexp
| Mul of aexp * aexp
| Div of aexp * aexp 
| Mod of aexp * aexp 

type bexp =
| Bool of bool 
| Eq of aexp * aexp
| Le of aexp * aexp
| Neg of bexp
| Conj of bexp * bexp 

type cmd =
| Assign of var * aexp
| Skip
| Seq of cmd * cmd
| If of bexp * cmd * cmd
| While of bexp * cmd
| Print of aexp 

type program = cmd 