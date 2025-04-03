exception NotImplemented;;

let rec hp : 'a list -> 'a list -> 'a list
  = fun l1 l2 -> match l1 with
    | [] -> l2
    | hd::tl -> if (List.exists (fun x -> if x=hd then true else false) l2) then hp tl l2 else hp tl (List.rev(hd::(List.rev l2)));;

(* let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> hp (l2@l1) [];; *)
  
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> hp l1 l2;;