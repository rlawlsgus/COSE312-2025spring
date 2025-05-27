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
  let le i1 i2 =
    match i1, i2 with
    | Bot, _ | _, Bot -> Bot
    | Range (Int c, Int c'), Range (l, u) when c = c' ->
        if leq (Int c) u then Range (max_int l (Int c), u) else Bot
    | Range (l, u), Range (Int c, Int c') when c = c' ->
        if leq l (Int c) then Range (l, min_int u (Int c)) else Bot
    | _ -> Bot
  let lt i1 i2 =
    match i1, i2 with
    | Bot, _ | _, Bot -> Bot
    | Range (Int c, Int c'), Range (l, u) when c = c' ->
        let min_x = Int (c + 1) in
        if leq min_x u then Range (max_int l min_x, u) else Bot
    | Range (l, u), Range (Int c, Int c') when c = c' ->
        let max_x = Int (c - 1) in
        if leq l max_x then Range (l, min_int u max_x) else Bot
    | _ -> Bot
  let ge i1 i2 =
    match i1, i2 with
    | Bot, _ | _, Bot -> Bot
    | Range (Int c, Int c'), Range (l, u) when c = c' ->
        if leq l (Int c) then Range (l, min_int u (Int c)) else Bot
    | Range (l, u), Range (Int c, Int c') when c = c' ->
        if leq (Int c) u then Range (max_int l (Int c), u) else Bot
    | _ -> Bot
  let gt i1 i2 =
    match i1, i2 with
    | Bot, _ | _, Bot -> Bot
    | Range (Int c, Int c'), Range (l, u) when c = c' ->
        let max_x = Int (c - 1) in
        if leq l max_x then Range (l, min_int u max_x) else Bot
    | Range (l, u), Range (Int c, Int c') when c = c' ->
        let min_x = Int (c + 1) in
        if leq min_x u then Range (max_int l min_x, u) else Bot
    | _ -> Bot
  let eq i1 i2 =
    match i1, i2 with
    | Bot, _ | _, Bot -> Bot
    | Range (Int c, Int c'), Range (l, u) when c = c' ->
        if leq l (Int c) && leq (Int c) u then Range (Int c, Int c) else Bot
    | Range (l, u), Range (Int c, Int c') when c = c' ->
        if leq l (Int c) && leq (Int c) u then Range (Int c, Int c) else Bot
    | _ -> Bot
  let not i =
    match i with
    | Bot -> Bot
    | Range (l, u) ->
      if l = Int 0 && u = Int 0 then Bot
      else if l = Int 0 then Range (Int 1, u)
      else if u = Int 0 then Range (l, Int (-1))
      else i
  let band i1 i2 =
    let not_zero i =
      match i with
      | Bot -> Bot
      | Range (l, u) ->
        if l = Int 0 && u = Int 0 then Bot
        else if l = Int 0 then Range (Int 1, u)
        else if u = Int 0 then Range (l, Int (-1))
        else i
    in
    eq (not_zero i1) (not_zero i2)
  let bor i1 i2 =
    match i1, i2 with
    | Bot, _ -> i2
    | _, Bot -> i1
    | Range (l1, u1), Range (l2, u2) ->
      let l = if leq l1 l2 then l1 else l2 in
      let u = if leq u1 u2 then u2 else u1 in
      Range (l, u)
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
    | S.ID x -> AbsMem.find (AbsLoc.from_var x) mem
    | S.ARR (x, _) ->
      let arr = AbsVal.get_absarray (AbsMem.find (AbsLoc.from_var x) mem) in
      let allocsites = AbsArray.get_allocsites arr in
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

let prune (cond : AbsVal.t) (mem : AbsMem.t) (exp : S.exp) : AbsMem.t =
  let rec get_var = function
    | S.LV (S.ID x) -> Some x
    | S.LT (e1, e2) | S.LE (e1, e2) | S.GT (e1, e2) | S.GE (e1, e2)
    | S.EQ (e1, e2) | S.AND (e1, e2) | S.OR (e1, e2) -> (
        match get_var e1 with
        | Some x -> Some x
        | None -> get_var e2
      )
    | S.NOT e -> get_var e
    | _ -> None
  in
  match get_var exp with
  | None -> mem
  | Some x ->
    let v = AbsMem.find (AbsLoc.from_var x) mem in
    let true_itv =
      let i = AbsVal.get_interval cond in
      Interval.meet i (Interval.one)
    in
    let new_v = AbsVal.meet_itv true_itv v in
    AbsMem.add (AbsLoc.from_var x) new_v mem

let fixpoint : Cfg.t -> Table.t
=fun cfg -> 
  let rec widening_phase worklist table visited =
    match worklist with
    | [] -> table
    | node :: rest ->
      let preds = Cfg.preds node cfg in
      let input = 
        NodeSet.fold (fun pred acc ->
          AbsMem.join acc (Table.find pred table)
        ) preds AbsMem.empty
      in
      let output = 
        match Node.get_instr node with
        | I_assume e -> 
          let v = eval e input in
          Table.print table;
          AbsVal.to_string v |> prerr_endline;
          let prune_v = prune v input e in
          AbsMem.print prune_v;
          input
        | I_assign (x, e) ->
          let v = eval e input in
          (match x with
          | S.ID x -> AbsMem.add (AbsLoc.from_var x) v input
          | _ -> input)
        | I_alloc (x, n) ->
          let v = AbsVal.from_absarr (AbsArray.create (Node.get_nodeid node) n) in
          AbsMem.add (AbsLoc.from_var x) v input
        | I_read x ->
          AbsMem.add (AbsLoc.from_var x) (AbsVal.from_itv (Interval.top)) input
        | I_print e ->
          let _ = eval e input in
          input
        | _ -> input
      in
      let old_output = Table.find node table in
      let new_output = AbsMem.widen old_output output in
      let succs = Cfg.succs node cfg in
      let new_worklist =
        if (AbsMem.order new_output old_output) && NodeSet.mem node visited then
          rest
        else
          NodeSet.elements succs @ rest
      in
      let new_table =
        if AbsMem.order new_output old_output then table
        else Table.add node new_output table
      in
      let new_visited = NodeSet.add node visited in
      widening_phase new_worklist new_table new_visited
  in

  let rec narrowing_phase worklist table visited =
    match worklist with
    | [] -> table
    | node :: rest ->
      let preds = Cfg.preds node cfg in
      let input = 
        NodeSet.fold (fun pred acc ->
          AbsMem.join acc (Table.find pred table)
        ) preds AbsMem.empty
      in
      let output = 
        match Node.get_instr node with
        | I_assume e -> 
          let v = eval e input in
          prune v input e
        | I_assign (x, e) ->
          let v = eval e input in
          (match x with
          | S.ID x -> AbsMem.add (AbsLoc.from_var x) v input
          | _ -> input)
        | I_alloc (x, n) ->
          let v = AbsVal.from_absarr (AbsArray.create (Node.get_nodeid node) n) in
          AbsMem.add (AbsLoc.from_var x) v input
        | I_read x ->
          AbsMem.add (AbsLoc.from_var x) (AbsVal.from_itv (Interval.top)) input
        | I_print e ->
          let _ = eval e input in
          input
        | _ -> input
      in
      let old_output = Table.find node table in
      let new_output = AbsMem.narrow old_output output in
      let succs = Cfg.succs node cfg in
      let new_worklist =
        if AbsMem.order old_output new_output && NodeSet.mem node visited then
          rest
        else
          NodeSet.elements succs @ rest
      in
      let new_table =
        if AbsMem.order old_output new_output then table
        else Table.add node new_output table
      in
      let new_visited = NodeSet.add node visited in
      narrowing_phase new_worklist new_table new_visited
  in

  let initial_table = Table.init (Cfg.nodesof cfg) in
  let entry = Cfg.get_entry cfg in
  let worklist = [entry] in
  let visited = NodeSet.empty in
  
  let after_widening = widening_phase worklist initial_table visited in
  Table.print after_widening;
  let after_narrowing = narrowing_phase worklist after_widening visited in
  Table.print after_narrowing;
  after_narrowing

let inspect : Cfg.t -> Table.t -> bool 
=fun _ _ -> true 

let analyze : Cfg.t -> bool 
=fun cfg -> 
  cfg 
  |> fixpoint  
  |> inspect cfg 