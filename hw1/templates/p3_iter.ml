exception NotImplemented;;

let iter : int * (int -> int) -> (int -> int)
= fun (n, f)  -> raise NotImplemented;; (* TODO *)