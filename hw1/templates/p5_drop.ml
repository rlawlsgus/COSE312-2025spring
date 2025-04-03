exception NotImplemented;;

let rec drop : 'a list -> int -> 'a list
= fun lst n -> if n = 0 then lst else match lst with
| [] -> []
| hd::tl -> drop tl (n-1);;
