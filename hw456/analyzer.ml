open G 

exception NotImplemented

module type Interval = sig
  type integer = PlusInf | MinusInf | Int of int 
  type t = Bot | Range of integer * integer 
  val bot : t
  val top : t 
  val one : t 
  val zero : t 
  val from_int : int -> t 
  val from_bounds : integer -> integer -> t
  val order : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val eq : t -> t -> t
  val not : t -> t
  val le : t -> t -> t
  val lt : t -> t -> t 
  val ge : t -> t -> t 
  val gt : t -> t -> t 
  val band : t -> t -> t
  val bor : t -> t -> t
  val to_string : t -> string
end

module Interval : Interval = struct
  type integer = PlusInf | MinusInf | Int of int 
  type t = Bot | Range of integer * integer 
  let bot = Bot 
  let top = Range (MinusInf, PlusInf)
  let one = Range (Int 1, Int 1)
  let zero = Range (Int 0, Int 0)
  let string_of_integer i = 
    match i with
    | PlusInf -> "+oo"
    | MinusInf -> "-oo"
    | Int n -> string_of_int n 
  let to_string i = 
    match i with 
    | Bot -> "Bot"
    | Range (i1, i2) -> "[" ^ string_of_integer i1 ^ ", " ^ string_of_integer i2 ^ "]"
  let from_int n = Range (Int n, Int n)
  let from_bounds i1 i2 = Range (i1, i2)

  let leq int1 int2 =
    match int1, int2 with
    | MinusInf, _ -> true
    | _, MinusInf -> false
    | Int n1, Int n2 -> n1 <= n2
    | _, PlusInf -> true
    | PlusInf, _ -> false

  let mul_int int1 int2 =
    match int1, int2 with
    | MinusInf, Int i -> if i < 0 then MinusInf else if i = 0 then Int 0 else PlusInf
    | Int i, MinusInf -> if i < 0 then MinusInf else if i = 0 then Int 0 else PlusInf
    | Int i1, Int i2 -> Int (i1 * i2)
    | PlusInf, Int i -> if i < 0 then MinusInf else if i = 0 then Int 0 else PlusInf
    | Int i, PlusInf -> if i < 0 then MinusInf else if i = 0 then Int 0 else PlusInf
    | PlusInf, PlusInf -> PlusInf
    | MinusInf, PlusInf -> MinusInf
    | PlusInf, MinusInf -> MinusInf
    | MinusInf, MinusInf -> PlusInf

  let neg_int i =
    match i with
    | PlusInf -> MinusInf
    | MinusInf -> PlusInf
    | Int n -> Int (-n)
  let neg_interval i =
    match i with
    | Bot -> Bot
    | Range (l, u) -> Range (neg_int u, neg_int l)
  
  let inverse_int i =
    match i with
    | Int n ->
      if n = 0 then PlusInf
      else if n = 1 then Int 1
      else if n = -1 then Int (-1)
      else Int 0
    | PlusInf -> Int 0
    | MinusInf -> Int 0

  let order i1 i2 = 
    match i1, i2 with
    | Bot, _ -> true
    | _, Bot -> false
    | Range (l1, u1), Range (l2, u2) ->
      leq l2 l1 && leq u1 u2

  let join i1 i2 = 
    match i1, i2 with
    | Bot, _ -> i2
    | _, Bot -> i1
    | Range (l1, u1), Range (l2, u2) ->
      let l = if leq l1 l2 then l1 else l2 in
      let u = if leq u1 u2 then u2 else u1 in
      Range (l, u)
  let meet i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let l = if leq l1 l2 then l2 else l1 in
      let u = if leq u1 u2 then u1 else u2 in
      if leq u l then Range (l, u) else Bot

  let widen i1 i2 = 
    match i1, i2 with
    | Bot, _ -> i2
    | _, Bot -> i1
    | Range (l1, u1), Range (l2, u2) ->
      let l = if leq l1 l2 then l1 else MinusInf in
      let u = if leq u1 u2 then u2 else PlusInf in
      Range (l, u)
  let narrow i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let l = if l1 = MinusInf then l2 else l1 in
      let u = if u1 = PlusInf then u2 else u1 in
      Range (l, u)

  let add i1 i2 =
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let l =
        match l1, l2 with
        | MinusInf, PlusInf
        | PlusInf, MinusInf -> MinusInf
        | MinusInf, _ | _, MinusInf -> MinusInf
        | PlusInf, _ | _, PlusInf -> PlusInf
        | Int i1_val, Int i2_val -> Int (i1_val + i2_val)
      in
      let u =
        match u1, u2 with
        | MinusInf, PlusInf
        | PlusInf, MinusInf -> PlusInf
        | MinusInf, _ | _, MinusInf -> MinusInf
        | PlusInf, _ | _, PlusInf -> PlusInf
        | Int i1_val, Int i2_val -> Int (i1_val + i2_val)
      in
      Range (l, u)
  let mul i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      let l =
        let pairs = [mul_int l1 l2; mul_int l1 u2; mul_int u1 l2; mul_int u1 u2] in
        let min a b = if leq a b then a else b in
        List.fold_left min PlusInf pairs
      in
      let u =
        let pairs = [mul_int l1 l2; mul_int l1 u2; mul_int u1 l2; mul_int u1 u2] in
        let max a b = if leq a b then b else a in
        List.fold_left max MinusInf pairs
      in
      Range (l, u)
  let sub i1 i2 =
  match i1, i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Range (l1, u1), Range (l2, u2) ->
    let neg_i2 = neg_interval (Range (l2, u2)) in
    add (Range (l1, u1)) neg_i2
  let div i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if leq l2 (Int 0) && leq (Int 0) u2 then
        top
      else
        let inv_l2_val = inverse_int u2 in
        let inv_u2_val = inverse_int l2 in
        let inv_i2_range = Range (inv_l2_val, inv_u2_val) in
        mul (Range (l1, u1)) inv_i2_range

  let eq i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if l1 = l2 && u1 = u2 && l1 = u1 then
        one
      else if meet i1 i2 = Bot then
        zero
      else
        top
  let le i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if leq u1 l2 then
        one
      else if (leq l1 u2) = false then
        zero
      else
        top
  let lt i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if (leq l2 u1) = false then
        one
      else if leq u2 l1 then
        zero
      else
        top
  let ge i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if leq u2 l1 then
        one
      else if (leq u1 l2) = false then
        zero
      else
        top
  let gt i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if (leq l1 u2) = false then
        one
      else if leq u1 l2 then
        zero
      else
        top
  let not i =
    match i with
    | Bot -> Bot
    | Range (Int 1, Int 1) -> zero
    | Range (Int 0, Int 0) -> one
    | Range _ -> top
  let band i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (Int 1, Int 1), Range (Int 1, Int 1) -> one
    | Range (Int 0, Int 0), Range _ -> zero
    | Range _, Range (Int 0, Int 0) -> zero
    | Range _, Range _ -> top
  let bor i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (Int 0, Int 0), Range (Int 0, Int 0) -> zero
    | Range (Int 1, Int 1), Range _ -> one
    | Range _, Range (Int 1, Int 1) -> one
    | Range _, Range _ -> top
end

type allocsite = int 
let string_of_allocsite l = "l" ^ string_of_int l

module AbsLoc = struct
  type t = Var of string | Allocsite of allocsite 
  let from_var x = Var x 
  let from_allocsite a = Allocsite a 
  let to_string a = 
    match a with 
    | Var x -> x 
    | Allocsite l -> string_of_allocsite l  
end 

module AbsArray = struct
  type t = allocsite BatSet.t * Interval.t 
  let bot = (BatSet.empty, Interval.bot)
  let get_allocsites (a, _) = a
  let get_size (_, sz) = sz
  let create l n = (BatSet.singleton l, Interval.from_int n)
  let join (a1, sz1) (a2, sz2) = (BatSet.union a1 a2, Interval.join sz1 sz2)
  let widen (a1, sz1) (a2, sz2) = (BatSet.union a1 a2, Interval.widen sz1 sz2)
  let narrow (a1, sz1) (a2, sz2) = (BatSet.intersect a1 a2, Interval.narrow sz1 sz2)
  let order (a1, sz1) (a2, sz2) = BatSet.subset a1 a2 && Interval.order sz1 sz2
  let to_string : t -> string 
  =fun (a, sz) -> Printf.sprintf "(%s, %s)" (Utils.string_of_set string_of_allocsite a) (Interval.to_string sz)
end 

module AbsVal = struct
  type t = Interval.t * AbsArray.t
  let bot = (Interval.bot, AbsArray.bot)
  let from_itv itv = (itv, AbsArray.bot)
  let from_absarr a = (Interval.bot, a)
  let get_interval (i, _) = i 
  let get_absarray (_, a) = a
  let join (i1, a1) (i2, a2) = (Interval.join i1 i2, AbsArray.join a1 a2)
  let meet_itv itv (i, a) = (Interval.meet itv i, a)
  let widen (i1, a1) (i2, a2) = (Interval.widen i1 i2, AbsArray.widen a1 a2)
  let narrow (i1, a1) (i2, a2) = (Interval.narrow i1 i2, AbsArray.narrow a1 a2)
  let order (i1, a1) (i2, a2) = Interval.order i1 i2 && AbsArray.order a1 a2
  let add (i1, a1) (i2, a2) = (Interval.add i1 i2, AbsArray.join a1 a2)
  let mul (i1, a1) (i2, a2) = (Interval.mul i1 i2, AbsArray.join a1 a2)
  let sub (i1, a1) (i2, a2) = (Interval.sub i1 i2, AbsArray.join a1 a2)
  let div (i1, a1) (i2, a2) = (Interval.div i1 i2, AbsArray.join a1 a2)
  let le (i1, a1) (i2, a2) = (Interval.le i1 i2, AbsArray.join a1 a2)
  let lt (i1, a1) (i2, a2) = (Interval.lt i1 i2, AbsArray.join a1 a2)
  let ge (i1, a1) (i2, a2) = (Interval.ge i1 i2, AbsArray.join a1 a2)
  let gt (i1, a1) (i2, a2) = (Interval.gt i1 i2, AbsArray.join a1 a2)
  let eq (i1, a1) (i2, a2) = (Interval.eq i1 i2, AbsArray.join a1 a2)
  let not (i, a) = (Interval.not i, a)
  let band (i1, a1) (i2, a2) = (Interval.band i1 i2, AbsArray.join a1 a2)
  let bor (i1, a1) (i2, a2) = (Interval.bor i1 i2, AbsArray.join a1 a2)
  let to_string : t -> string 
  =fun (itv, arr) -> Printf.sprintf "(%s, %s)" (Interval.to_string itv) (AbsArray.to_string arr)
end 

module type AbsMem = sig
  type t = (AbsLoc.t, AbsVal.t) BatMap.t
  val empty : t
  val find : AbsLoc.t -> t -> AbsVal.t 
  val find_set : AbsLoc.t BatSet.t -> t -> AbsVal.t 
  val add : AbsLoc.t -> AbsVal.t -> t -> t
  val add_set : AbsLoc.t BatSet.t -> AbsVal.t -> t -> t 
  val join : t -> t -> t 
  val widen : t -> t -> t 
  val narrow : t -> t -> t
  val order : t -> t -> bool 
  val print : t -> unit 
end

module AbsMem : AbsMem = struct
  type t = (AbsLoc.t, AbsVal.t) BatMap.t
  let empty = BatMap.empty
  let find x m = try BatMap.find x m with _ -> AbsVal.bot
  let find_set xs m = BatSet.fold (fun x -> AbsVal.join (find x m)) xs AbsVal.bot 
  let strong_update loc v m = BatMap.add loc v m 
  let weak_update loc v m = BatMap.add loc (AbsVal.join (find loc m) v) m
  let add loc v m = 
    match loc with 
    | AbsLoc.Var _ -> strong_update loc v m 
    | AbsLoc.Allocsite _ -> weak_update loc v m 
  let add_set locs v m = 
    if BatSet.cardinal locs = 1 then add (BatSet.choose locs) v m 
    else BatSet.fold (fun loc -> weak_update loc v) locs m 
  let join m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.join v (find x m')) m') m1 m2
  let widen m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.widen v (find x m')) m') m1 m2
  let narrow m1 m2 = BatMap.foldi (fun x v m' -> add x (AbsVal.narrow v (find x m')) m') m1 m2
  let order m1 m2 = BatMap.for_all (fun x v -> AbsVal.order v (find x m2)) m1
  let print m = 
    BatMap.iter (fun x v -> 
      prerr_endline (AbsLoc.to_string x ^ " |-> " ^ AbsVal.to_string v)
    ) m 
end

module type Table = sig
  type t = AbsMem.t NodeMap.t
  val empty : t
  val init : Node.t list -> t 
  val find : Node.t -> t -> AbsMem.t 
  val add : Node.t -> AbsMem.t -> t -> t
  val print : t -> unit
end 

module Table : Table = struct 
  type t = AbsMem.t NodeMap.t
  let empty = NodeMap.empty 
  let add = NodeMap.add
  let init ns = List.fold_right (fun n -> add n AbsMem.empty) ns empty
  let find : Node.t -> t -> AbsMem.t 
  =fun n t -> try NodeMap.find n t with _ -> AbsMem.empty
  let print t = NodeMap.iter (fun n m -> 
    prerr_endline (string_of_int (Node.get_nodeid n)); 
    AbsMem.print m; 
    prerr_endline "") t  
end

let fixpoint : Cfg.t -> Table.t
=fun _ -> Table.empty

let inspect : Cfg.t -> Table.t -> bool 
=fun _ _ -> true 

let analyze : Cfg.t -> bool 
=fun cfg -> 
  cfg 
  |> fixpoint  
  |> inspect cfg 