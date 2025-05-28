open G 

exception NotImplemented
exception DivByZero
exception OutOfBounds
exception TypeMismatch
exception InfiniteLoop
exception UnassignedVariable

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
    | Int n1, Int n2 -> n1 <= n2
    | _, PlusInf -> true
    | PlusInf, _ -> false
    | _ -> false

  let ltn int1 int2 =
    match int1, int2 with
    | MinusInf, _ -> true
    | Int n1, Int n2 -> n1 < n2
    | _, PlusInf -> true
    | PlusInf, _ -> false
    | _ -> false

  let max_int a b = if leq a b then b else a
  let min_int a b = if leq a b then a else b

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
  
  let div_int int1 int2 =
    match int1, int2 with
    | MinusInf, Int i -> if i < 0 then PlusInf else MinusInf
    | Int i1, Int i2 -> Int (i1 / i2)
    | PlusInf, Int i -> if i < 0 then MinusInf else PlusInf
    | _, PlusInf -> Int 0
    | _, MinusInf -> Int 0

  let neg_int i =
    match i with
    | PlusInf -> MinusInf
    | MinusInf -> PlusInf
    | Int n -> Int (-n)
  let neg_interval i =
    match i with
    | Bot -> Bot
    | Range (l, u) -> Range (neg_int u, neg_int l)

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
      if leq l u then Range (l, u) else Bot

  let widen i1 i2 =
    match i1, i2 with
    | Bot, _ -> i2
    | _, Bot -> i1
    | Range (l1, u1), Range (l2, u2) ->
      let l = if ltn l2 l1 then MinusInf else l1 in
      let u = if ltn u1 u2 then PlusInf else u1 in
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
      let pairs = [mul_int l1 l2; mul_int l1 u2; mul_int u1 l2; mul_int u1 u2] in
      let l =
        List.fold_left min_int PlusInf pairs
      in
      let u =
        List.fold_left max_int MinusInf pairs
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
        raise DivByZero
      else
        let pairs = [div_int l1 l2; div_int l1 u2; div_int u1 l2; div_int u1 u2] in
        let l =
          List.fold_left min_int PlusInf pairs
        in
        let u =
          List.fold_left max_int MinusInf pairs
        in
        Range (l, u)

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
      else if ltn u2 l1 then
        zero
      else
        top

  let lt i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if ltn u1 l2 then
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
      else if ltn u1 l2 then
        zero
      else
        top

  let gt i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (l1, u1), Range (l2, u2) ->
      if ltn u2 l1 then
        one
      else if leq u1 l2 then
        zero
      else
        top

  let not i =
    match i with
    | Bot -> Bot
    | Range (Int 0, Int 0) -> one
    | Range (l, u) ->
      if leq l (Int 0) && leq (Int 0) u then
        top
      else
        zero

  let band i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (Int 0, Int 0), Range _ -> zero
    | Range _, Range (Int 0, Int 0) -> zero
    | Range (l1, u1), Range (l2, u2) ->
      if leq l1 (Int 0) && leq (Int 0) u1 || leq l2 (Int 0) && leq (Int 0) u2 then
        top
      else
        one

  let bor i1 i2 = 
    match i1, i2 with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Range (Int 0, Int 0), Range (Int 0, Int 0) -> zero
    | Range (l1, u1), Range (l2, u2) ->
      if leq l1 (Int 0) && leq (Int 0) u1 && leq l2 (Int 0) && leq (Int 0) u2 then
        top
      else
        one
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

let rec eval : S.exp -> AbsMem.t -> AbsVal.t
=fun e mem ->
  match e with
  | S.NUM n -> AbsVal.from_itv (Interval.from_int n)
  | S.LV lv ->
    (match lv with
    | S.ID x ->
      let v = AbsMem.find (AbsLoc.from_var x) mem in
      if AbsVal.get_absarray v <> AbsArray.bot then
        raise TypeMismatch;
      if v = AbsVal.bot then
        raise UnassignedVariable
      else v
    | S.ARR (x, e) ->
      let arr_val = AbsMem.find (AbsLoc.from_var x) mem in
      if arr_val = AbsVal.bot then raise UnassignedVariable;
      let arr = AbsVal.get_absarray arr_val in
      let allocsites = AbsArray.get_allocsites arr in
      let size = AbsArray.get_size arr in
      let idx = eval e mem in
      let idx_itv = AbsVal.get_interval idx in
      let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
      if Interval.lt idx_itv (Interval.from_int 0) <> Interval.zero ||
         Interval.ge idx_itv size_itv <> Interval.zero then
        raise OutOfBounds
      else
        let locs = BatSet.map (fun a -> AbsLoc.from_allocsite a) allocsites in
        AbsMem.find_set locs mem)
  | S.ADD (e1, e2) -> AbsVal.add (eval e1 mem) (eval e2 mem)
  | S.SUB (e1, e2) -> AbsVal.sub (eval e1 mem) (eval e2 mem)
  | S.MUL (e1, e2) -> AbsVal.mul (eval e1 mem) (eval e2 mem)
  | S.DIV (e1, e2) -> AbsVal.div (eval e1 mem) (eval e2 mem)
  | S.MINUS e -> AbsVal.sub (AbsVal.from_itv (Interval.from_int 0)) (eval e mem)
  | S.NOT e -> AbsVal.not (eval e mem)
  | S.LT (e1, e2) -> AbsVal.lt (eval e1 mem) (eval e2 mem)
  | S.LE (e1, e2) -> AbsVal.le (eval e1 mem) (eval e2 mem)
  | S.GT (e1, e2) -> AbsVal.gt (eval e1 mem) (eval e2 mem)
  | S.GE (e1, e2) -> AbsVal.ge (eval e1 mem) (eval e2 mem)
  | S.EQ (e1, e2) -> AbsVal.eq (eval e1 mem) (eval e2 mem)
  | S.AND (e1, e2) -> AbsVal.band (eval e1 mem) (eval e2 mem)
  | S.OR (e1, e2) -> AbsVal.bor (eval e1 mem) (eval e2 mem)

let rec prune (mem : AbsMem.t) (exp : S.exp) : AbsMem.t =
  match exp with
  | S.LT (S.LV lv, e2) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf (Int (u2-1)))
            | Interval.Range (_, u2) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf u2)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e1) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e1 mem in
          let idx_itv = AbsVal.get_interval idx in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> 
              if u2 <= 0 then Interval.bot
              else Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf (Int (u2-1)))
            | Interval.Range (_, u2) -> Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf u2)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.LT (e1, S.LV lv) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> Interval.meet itv (Interval.from_bounds (Int (l1+1)) Interval.PlusInf)
            | Interval.Range (l1, _) -> Interval.meet itv (Interval.from_bounds l1 Interval.PlusInf)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e2) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e2 mem in
          let idx_itv = AbsVal.get_interval idx in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> 
              if l1 >= 0 then Interval.meet idx_itv (Interval.from_bounds (Int (l1+1)) Interval.PlusInf)
              else idx_itv
            | Interval.Range (l1, _) -> Interval.meet idx_itv (Interval.from_bounds l1 Interval.PlusInf)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.LE (S.LV lv, e2) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf (Int u2))
            | Interval.Range (_, u2) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf u2)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e1) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e1 mem in
          let idx_itv = AbsVal.get_interval idx in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> 
              if u2 < 0 then Interval.bot
              else Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf (Int u2))
            | Interval.Range (_, u2) -> Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf u2)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.LE (e1, S.LV lv) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> Interval.meet itv (Interval.from_bounds (Int l1) Interval.PlusInf)
            | Interval.Range (l1, _) -> Interval.meet itv (Interval.from_bounds l1 Interval.PlusInf)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e2) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e2 mem in
          let idx_itv = AbsVal.get_interval idx in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> 
              if l1 > 0 then Interval.meet idx_itv (Interval.from_bounds (Int l1) Interval.PlusInf)
              else idx_itv
            | Interval.Range (l1, _) -> Interval.meet idx_itv (Interval.from_bounds l1 Interval.PlusInf)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.GT (S.LV lv, e2) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> Interval.meet itv (Interval.from_bounds (Int (u2+1)) Interval.PlusInf)
            | Interval.Range (_, u2) -> Interval.meet itv (Interval.from_bounds u2 Interval.PlusInf)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e1) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e1 mem in
          let idx_itv = AbsVal.get_interval idx in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> 
              if u2 < 0 then Interval.bot
              else Interval.meet idx_itv (Interval.from_bounds (Int (u2+1)) Interval.PlusInf)
            | Interval.Range (_, u2) -> Interval.meet idx_itv (Interval.from_bounds u2 Interval.PlusInf)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.GT (e1, S.LV lv) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf (Int (l1-1)))
            | Interval.Range (l1, _) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf l1)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e2) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e2 mem in
          let idx_itv = AbsVal.get_interval idx in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> 
              if l1 > 0 then Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf (Int (l1-1)))
              else idx_itv
            | Interval.Range (l1, _) -> Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf l1)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.GE (S.LV lv, e2) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> Interval.meet itv (Interval.from_bounds (Int u2) Interval.PlusInf)
            | Interval.Range (_, u2) -> Interval.meet itv (Interval.from_bounds u2 Interval.PlusInf)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e1) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e1 mem in
          let idx_itv = AbsVal.get_interval idx in
          let right_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned =
            match right_itv with
            | Interval.Range (_, Interval.Int u2) -> 
              if u2 < 0 then Interval.bot
              else Interval.meet idx_itv (Interval.from_bounds (Int u2) Interval.PlusInf)
            | Interval.Range (_, u2) -> Interval.meet idx_itv (Interval.from_bounds u2 Interval.PlusInf)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.GE (e1, S.LV lv) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf (Int l1))
            | Interval.Range (l1, _) -> Interval.meet itv (Interval.from_bounds Interval.MinusInf l1)
            | _ -> itv
          in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e2) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e2 mem in
          let idx_itv = AbsVal.get_interval idx in
          let left_itv = AbsVal.get_interval (eval e1 mem) in
          let pruned =
            match left_itv with
            | Interval.Range (Interval.Int l1, _) -> 
              if l1 > 0 then Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf (Int l1))
              else idx_itv
            | Interval.Range (l1, _) -> Interval.meet idx_itv (Interval.from_bounds Interval.MinusInf l1)
            | _ -> idx_itv
          in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.EQ (S.LV lv, e2)
  | S.EQ (e2, S.LV lv) ->
      (match lv with
      | S.ID x ->
          let v = AbsMem.find (AbsLoc.from_var x) mem in
          let itv = AbsVal.get_interval v in
          let other_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned = Interval.meet itv other_itv in
          AbsMem.add (AbsLoc.from_var x) (AbsVal.meet_itv pruned v) mem
      | S.ARR (x, e1) ->
          let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
          let size = AbsArray.get_size arr in
          let idx = eval e1 mem in
          let idx_itv = AbsVal.get_interval idx in
          let other_itv = AbsVal.get_interval (eval e2 mem) in
          let pruned = Interval.meet idx_itv other_itv in
          let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
          if Interval.lt pruned (Interval.from_int 0) <> Interval.zero || 
             Interval.ge pruned size_itv <> Interval.zero then
            raise OutOfBounds
          else
            mem)
  | S.NOT e -> 
    prune mem (negate_exp e)
  | S.AND (e1, e2) ->
      let mem' = prune mem e1 in
      prune mem' e2
  | S.OR (e1, e2) ->
      let mem1 = prune mem e1 in
      let mem2 = prune mem e2 in
      AbsMem.join mem1 mem2
  | _ -> mem

and negate_exp exp =
  match exp with
  | S.LT (a, b) -> S.GE (a, b)
  | S.LE (a, b) -> S.GT (a, b)
  | S.GT (a, b) -> S.LE (a, b)
  | S.GE (a, b) -> S.LT (a, b)
  | S.EQ (a, b) -> S.OR (S.LT (a, b), S.GT (a, b))
  | S.NOT e -> e
  | S.OR (e1, e2) -> S.AND (negate_exp e1, negate_exp e2)
  | S.AND (e1, e2) -> S.OR (negate_exp e1, negate_exp e2)
  | _ -> exp

  let fixpoint : Cfg.t -> Table.t =
    fun cfg ->
      let nodes = Cfg.nodesof cfg in
      let entry = Cfg.get_entry cfg in
  
      let rec widening_phase worklist table =
        match worklist with
        | [] -> table
        | n :: rest ->
          let preds = Cfg.preds n cfg in
          let input =
            NodeSet.fold (fun pred acc -> AbsMem.join acc (Table.find pred table)) preds AbsMem.empty
          in
          let s =
            match Node.get_instr n with
            | I_assume e ->
              let pruned = prune input e in
              let cond = eval e input in
              if AbsVal.get_interval cond = Interval.zero then
                AbsMem.empty
              else
                pruned
            | I_assign (x, e_assign) ->
              let v = eval e_assign input in
              (match x with
              | S.ID x_id -> AbsMem.add (AbsLoc.from_var x_id) v input
              | S.ARR (x_arr, e_idx) ->
                let arr_val = AbsMem.find (AbsLoc.from_var x_arr) input in
                let arr = AbsVal.get_absarray arr_val in
  
                let allocsites = AbsArray.get_allocsites arr in
                let size = AbsArray.get_size arr in
                let idx = eval e_idx input in
                let idx_itv = AbsVal.get_interval idx in
  
                let size_itv = AbsVal.get_interval (AbsVal.from_itv size) in
                if Interval.lt idx_itv (Interval.from_int 0) <> Interval.zero ||
                   Interval.ge idx_itv size_itv <> Interval.zero then
                  raise OutOfBounds
                else
                  let locs = BatSet.map (fun a -> AbsLoc.from_allocsite a) allocsites in
                  AbsMem.add_set locs v input)
            | I_alloc (x, size) ->
              let v = AbsVal.from_absarr (AbsArray.create (Node.get_nodeid n) size) in
              AbsMem.add (AbsLoc.from_var x) v input
            | I_read x ->
              AbsMem.add (AbsLoc.from_var x) (AbsVal.from_itv Interval.top) input
            | I_print e -> let _ = eval e input in input
            | I_skip -> input
          in
          let old = Table.find n table in
          let new_x =
            if Cfg.is_loophead n cfg then
              AbsMem.widen old s
            else
              AbsMem.join old s
          in
          let changed = not (AbsMem.order new_x old && AbsMem.order old new_x) in
          let succs = Cfg.succs n cfg |> NodeSet.elements in
          let new_worklist =
            if changed then
              List.fold_left (fun wl n' -> if List.mem n' wl then wl else n' :: wl) rest succs
            else
              rest
          in
          let new_table = if changed then Table.add n new_x table else table in
          widening_phase new_worklist new_table
      in
  
      let rec narrowing_phase worklist table =
        match worklist with
        | [] -> table
        | n :: rest ->
          let preds = Cfg.preds n cfg in
          let input =
            NodeSet.fold (fun pred acc -> AbsMem.join acc (Table.find pred table)) preds AbsMem.empty
          in
          let s =
            match Node.get_instr n with
            | I_assume e ->
              let pruned = prune input e in
              let cond = eval e input in
              if AbsVal.get_interval cond = Interval.zero then
                AbsMem.empty
              else
                pruned
            | I_assign (x, e_assign) ->
              let v = eval e_assign input in
              (match x with
              | S.ID x_id -> AbsMem.add (AbsLoc.from_var x_id) v input
              | S.ARR (x_arr, e_idx) ->
                let _ = eval e_idx input in
                let arr_val = AbsMem.find (AbsLoc.from_var x_arr) input in
                let arr = AbsVal.get_absarray arr_val in
  
                let allocsites = AbsArray.get_allocsites arr in
                let locs = BatSet.map (fun a -> AbsLoc.from_allocsite a) allocsites in
                  AbsMem.add_set locs v input)
            | I_alloc (x, size) ->
              let v = AbsVal.from_absarr (AbsArray.create (Node.get_nodeid n) size) in
              AbsMem.add (AbsLoc.from_var x) v input
            | I_read x ->
              AbsMem.add (AbsLoc.from_var x) (AbsVal.from_itv Interval.top) input
            | I_print e -> let _ = eval e input in input
            | _ -> input
          in
          let old = Table.find n table in
          let new_x =
            if Cfg.is_loophead n cfg then
              AbsMem.narrow old s
            else
              s
          in
          let changed = not (AbsMem.order new_x old && AbsMem.order old new_x) in
          let succs = Cfg.succs n cfg |> NodeSet.elements in
          let new_worklist =
            if changed then
              List.fold_left (fun wl n' -> if List.mem n' wl then wl else n' :: wl) rest succs
            else
              rest
          in
          let new_table = if changed then Table.add n new_x table else table in
          narrowing_phase new_worklist new_table
      in
  
      let initial_table = Table.init nodes in
      let initial_worklist_widening = entry :: (Cfg.succs (entry) cfg |> NodeSet.elements) in
      let after_widening = widening_phase initial_worklist_widening initial_table in
      prerr_endline "After Widening:";
      Table.print after_widening;
      let rec repeat_narrowing n table =
        if n = 0 then table
        else
          let initial_worklist_narrowing = Cfg.nodesof cfg in
          let after_narrowing = narrowing_phase initial_worklist_narrowing table in
          prerr_endline ("After Narrowing " ^ string_of_int (4 - n) ^ ":");
          Table.print after_narrowing;
          repeat_narrowing (n - 1) after_narrowing
      in
      let after_narrowing = repeat_narrowing 3 after_widening in
      Table.print after_narrowing;
      after_narrowing

let inspect : Cfg.t -> Table.t -> bool 
=fun cfg table -> 
  prerr_endline "Inspecting CFG...";
  (* 루프 헤드 노드 검사 *)
  let entry = Cfg.get_entry cfg in
  let rec dfs visited node =
    if List.mem node visited then true
    else
      let visited' = node :: visited in
      let mem = Table.find node table in
      (* 현재 노드 처리 *)
      Node.to_string node |> prerr_endline;
      match Node.get_instr node with
      | I_assume e -> 
        let cond = eval e mem in
        let cond_itv = AbsVal.get_interval cond in
        (match cond_itv with
          | Interval.Range (Int 1, Int 1) -> 
            visit_successors visited' node  (* 일반 assume 참 처리 *)
          | Interval.Range (Int 0, Int 0) -> 
            true  (* 데드코드 제외 *)
          | _ -> 
            visit_successors visited' node)
      | I_assign (_, e) -> 
        let _ = eval e mem in
        visit_successors visited' node
      | I_print e -> 
        let _ = eval e mem in
        visit_successors visited' node
      | _ -> 
        visit_successors visited' node
  and visit_successors visited node =
    let succs = Cfg.succs node cfg in
    let succ_list = NodeSet.elements succs in
    check_all_successors visited succ_list
  and check_all_successors visited = function
    | [] -> true
    | succ :: rest ->
      if dfs visited succ then 
        check_all_successors visited rest
      else 
        false
  in
  dfs [] entry

    
let analyze : Cfg.t -> bool 
=fun cfg -> 
  try
    cfg 
    |> fixpoint  
    |> inspect cfg 
  with
    | DivByZero -> 
      prerr_endline "Warning: Division by zero detected";
      false
    | OutOfBounds -> 
      prerr_endline "Warning: Array access out of bounds";
      false
    | TypeMismatch -> 
      prerr_endline "Warning: Type mismatch detected";
      false
    | InfiniteLoop -> 
      prerr_endline "Warning: Infinite loop detected";
      false
    | UnassignedVariable -> 
      prerr_endline "Warning: Unassigned variable detected";
      false
    | _ -> 
      prerr_endline "Warning: Unknown error occurred";
      false