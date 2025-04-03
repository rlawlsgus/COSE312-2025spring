exception NotImplemented;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
  | [] -> ([], [])
  | (a, b)::tl -> let (a', b') = unzip tl in (a::a', b::b');;