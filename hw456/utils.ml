let (<<<) f g = fun x -> f (g x)
let (>>>) f g = fun x -> g (f x)
let ($>) x f = match x with Some s -> f s | None -> None
let (&>) x f = match x with Some s -> Some (f s) | None -> None
let (@) l1 l2 = BatList.append l1 l2
let id x = x
let flip f = fun y x -> f x y
let cond c f g x = if c then f x else g x
let opt c f x = if c then f x else x
let tuple x = (x, x)

let compare_string = Base.compare_string
let compare_bool = Base.compare_bool
let compare_int = Base.compare_int

let domof m = BatMap.foldi (fun k _ set -> BatSet.add k set) m BatSet.empty

let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f list1 list2 init ->
  let f' acc a b = f a b acc in
  List.fold_left2 f' init list1 list2

let list_rev : 'a list -> 'a list
= fun l ->
  let rec list_rev_rec l1 l2 =
    match l1 with
    | [] -> l2
    | a :: b -> list_rev_rec b (a :: l2) in
  list_rev_rec l []

let list_replace : 'a list -> int -> 'a -> 'a list 
=fun l pos v -> List.mapi (fun i x -> if i = pos then v else x) l

let append_opt : 'a option -> 'a list -> 'a list
= fun x l ->
  match x with None -> l | Some x -> x::l
let find_opt : 'a -> ('a, 'b) BatMap.t -> 'b option
= fun k m ->
  try Some (BatMap.find k m) with
  | Not_found -> None

let find_def : 'a -> ('a, 'b) BatMap.t -> 'b -> 'b
= fun k m default ->
  try BatMap.find k m with _ -> default

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="(") ?(last=")") ?(sep=",") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let string_of_set ?(first="{") ?(last="}") ?(sep=",") : ('a -> string)
  -> ('a BatSet.t) -> string
= fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last

let string_of_map ?(first="{") ?(last="}") ?(sep=",\n") ?(indent="") : ('a -> string)
  -> ('b -> string) -> (('a, 'b) BatMap.t) -> string
= fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep (sep^indent) str acc in
  if BatMap.is_empty map then "empty"
  else indent ^ first ^ BatMap.foldi add_string_of_k_v map "" ^ last

let i2s = string_of_int

let list2set l = list_fold BatSet.add l BatSet.empty
let set2list s = BatSet.fold (fun x l -> x::l) s []

let rec zip a b = 
  match a, b with
  | [], [] -> []
  | h::t, [] -> (Some h, None)::(zip t [])
  | [], h'::t' -> (None, Some h')::(zip [] t') 
  | h::t, h'::t' -> (Some h, Some h')::(zip t t')

(* range 0 2 = [0; 1; 2] *)
let rec range b e = if b = e then [b] else b::(range (b+1) e)

(* upto 2 = [0; 1] *)
let rec upto n = 
  if n <= 0 then []
  else (upto (n-1))@[n-1]

(* take 2 [0; 1; 2] = [0; 1] *)
let rec take n l = 
  if n < 0 then raise (Failure "take")
  else if n = 0 then []
  else (List.hd l)::(take (n-1) (List.tl l))

(* drop 2 [0; 1; 2] = [2] *)
let rec drop n l = 
  if n = 0 then l 
  else 
    match l with 
    | [] -> []
    | _::tl -> drop (n-1) tl

(* sublist 1 3 [0; 1; 2; 3; 4] = [1; 2] *)
let sublist i j l = take (j-i) (drop i l)
