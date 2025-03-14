exception NotImplemented;;

let reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 c -> raise NotImplemented;; (* TODO *)