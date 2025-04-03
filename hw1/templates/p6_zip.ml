exception NotImplemented;;

let rec zip : int list * int list -> int list
= fun (l1, l2) -> match l1, l2 with
  | [], [] -> []
  | [], _ -> l2
  | _, [] -> l1
  | hd1::tl1, hd2::tl2 -> hd1::hd2::zip(tl1, tl2);;
