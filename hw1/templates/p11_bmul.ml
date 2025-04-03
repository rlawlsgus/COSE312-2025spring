exception NotImplemented;;

type digit = ZERO | ONE
type bin = digit list

let rec rmzero : bin -> bin
= fun b -> match b with
  | ZERO::tl -> rmzero tl
  | ONE::tl -> b
  | [] -> [ZERO]

let rec bpad : bin -> int -> bin
= fun b n -> if n = 0 then b
             else bpad (ZERO::b) (n-1)

let rec badd : bin -> bin -> digit -> bin
= fun l1 l2 carry -> match (l1, l2, carry) with
  | ([], [], ZERO) -> []
  | ([], [], ONE) -> [ONE]
  | (hd1::tl1, [], ZERO) | ([], hd1::tl1, ZERO) -> hd1::(badd tl1 [] ZERO)
  | (hd1::tl1, [], ONE) | ([], hd1::tl1, ONE) -> (if hd1 = ZERO then ONE else ZERO)::(badd tl1 [] (if hd1 = ZERO then ZERO else ONE))
  | (hd1::tl1, hd2::tl2, carry) ->
      let sum = (if hd1 = ONE then 1 else 0) + (if hd2 = ONE then 1 else 0) + (if carry = ONE then 1 else 0) in
      if sum = 0 then ZERO::(badd tl1 tl2 ZERO)
      else if sum = 1 then ONE::(badd tl1 tl2 ZERO)
      else if sum = 2 then ZERO::(badd tl1 tl2 ONE)
      else ONE::(badd tl1 tl2 ONE)

let bmul : bin -> bin -> bin
  = fun b1 b2 -> let rec bmul' : bin -> bin -> int -> bin -> bin
    = fun b1 b2 n result -> match b1 with
      | [] -> result
      | hd::tl -> if hd = ZERO then bmul' tl b2 (n+1) result
                  else bmul' tl b2 (n+1) (badd result (bpad b2 n) ZERO) in
    rmzero (List.rev (bmul' (List.rev b1) (List.rev b2) 0 [ZERO]));;

