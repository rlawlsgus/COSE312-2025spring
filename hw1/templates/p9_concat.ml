exception NotImplemented;;

let rec concat : 'a list list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd @ (concat tl);;