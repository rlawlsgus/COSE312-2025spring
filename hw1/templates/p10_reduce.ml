exception NotImplemented;;

let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 c -> match l1, l2 with
  | h1::t1, h2::t2 -> let z = f h1 h2 c in reduce f t1 t2 z
  | _, _ -> c;;

  (* 잘못된 입력에 대한 예외처리 필요? *)