open T

let didx = ref 0
let new_def() = didx := !didx + 1; !didx

let idx = ref 0
let new_idx() = idx := !idx + 1; !idx

type instr_info = {
  idx: int;
  label: int;
  def_id: int;
  instr: T.instr;
  use: T.var list;
  def: T.var list;
}

type liveness_info = {
  in_set: T.var BatSet.t;
  out_set: T.var BatSet.t;
}

type program_map = instr_info list
type liveness_map = liveness_info list

type const_value = 
  | Bottom
  | Const of int
  | Top

type const_table = (T.var, const_value) BatMap.t

type table_map = (int, const_table) BatMap.t

type analysis_result = {
  reaching_defs: (int, int BatSet.t) BatMap.t;
  liveness: liveness_map;
  constant_propagation: table_map;
}

let remove_skips : T.program -> T.program
=fun pgm ->
  let rec aux acc = function
    | [] -> List.rev acc
    | (label, T.SKIP)::rest ->
      (match label, rest with
      | 0, _ -> aux acc rest
      | l, (0, instr2)::rest2 -> aux acc ((l, instr2)::rest2)
      | l, (l2, instr2)::rest2 -> aux ((l, T.SKIP)::acc) ((l2, instr2)::rest2)
      | _ -> aux acc rest
      )
    | hd::rest -> aux (hd::acc) rest
  in
  aux [] pgm

let initial_setting : T.program -> program_map
=fun pgm ->
  List.map (fun (label, instr) ->
    let use, def, def_id = 
      match instr with
      | T.ASSIGNV (x, _, y, z) -> ([y; z], [x], new_def())
      | T.ASSIGNC (x, _, y, _) -> ([y], [x], new_def())
      | T.ASSIGNU (x, _, y) -> ([y], [x], new_def())
      | T.COPY (x, y) -> ([y], [x], new_def())
      | T.COPYC (x, _) -> ([], [x], new_def())
      | T.LOAD (x, (a, i)) -> ([a; i], [x], new_def())
      | T.STORE ((a, i), x) -> ([a; i; x], [], 0)
      | T.READ x -> ([], [x], new_def())
      | T.WRITE x -> ([x], [], 0)
      | T.CJUMP (x, _) | T.CJUMPF (x, _) -> ([x], [], 0)
      | _ -> ([], [], 0)
    in
    {idx = new_idx(); label; def_id; instr; use; def}
  ) pgm

let repair_use_def_chains : program_map -> analysis_result -> program_map
=fun pgm_map _ ->
  List.map (fun info ->
    let use, def =
      match info.instr with
      | T.ASSIGNV (x, _, y, z) -> ([y; z], [x])
      | T.ASSIGNC (x, _, y, _) -> ([y], [x])
      | T.ASSIGNU (x, _, y) -> ([y], [x])
      | T.COPY (x, y) -> ([y], [x])
      | T.COPYC (x, _) -> ([], [x])
      | T.LOAD (x, (a, i)) -> ([a; i], [x])
      | T.STORE ((a, i), x) -> ([a; i; x], [])
      | T.READ x -> ([], [x])
      | T.WRITE x -> ([x], [])
      | T.CJUMP (x, _) | T.CJUMPF (x, _) -> ([x], [])
      | _ -> ([], [])
    in
    {info with use; def}
  ) pgm_map

let print_pgm_map : program_map -> unit
=fun pgm_map ->
  let print_var_info info =
    Printf.printf "Idx: %d, Label: %d, Def ID: %d, Use: %s, Def: %s\n" 
      info.idx
      info.label 
      info.def_id
      (String.concat ", " info.use)
      (String.concat ", " info.def);
    T.pp [info.label, info.instr];
    print_endline "-------------------"
  in
  print_endline "\n=== Program Map ===";
  List.iter print_var_info pgm_map;
  print_endline "===================\n"

let eval_const : T.instr -> const_table -> const_value
=fun instr table ->
  let get_val var = 
    try BatMap.find var table 
    with Not_found -> Bottom
  in
  match instr with
  | T.COPYC (_, n) -> Const n
  | T.COPY (_, y) -> get_val y
  | T.READ _ -> Top
  | T.ASSIGNV (_, op, y, z) ->
    let vy = get_val y in
    let vz = get_val z in
    (match vy, vz with
    | Const n1, Const n2 ->
      (match op with
      | T.ADD -> Const (n1 + n2)
      | T.SUB -> Const (n1 - n2)
      | T.MUL -> Const (n1 * n2)
      | T.DIV -> Const (n1 / n2)
      | T.LT -> Const (if n1 < n2 then 1 else 0)
      | T.LE -> Const (if n1 <= n2 then 1 else 0)
      | T.GT -> Const (if n1 > n2 then 1 else 0)
      | T.GE -> Const (if n1 >= n2 then 1 else 0)
      | T.EQ -> Const (if n1 = n2 then 1 else 0)
      | T.AND -> Const (if n1 != 0 && n2 != 0 then 1 else 0)
      | T.OR -> Const (if n1 != 0 || n2 != 0 then 1 else 0))
    | Top, Const 0 | Const 0, Top when op = T.MUL -> Const 0
    | Top, _ | _, Top -> Top
    | _ -> Bottom)
  | T.ASSIGNC (_, op, y, n) ->
    let vy = get_val y in
    (match vy with
    | Const n1 ->
      (match op with
      | T.ADD -> Const (n1 + n)
      | T.SUB -> Const (n1 - n)
      | T.MUL -> Const (n1 * n)
      | T.DIV -> Const (n1 / n)
      | T.LT -> Const (if n1 < n then 1 else 0)
      | T.LE -> Const (if n1 <= n then 1 else 0)
      | T.GT -> Const (if n1 > n then 1 else 0)
      | T.GE -> Const (if n1 >= n then 1 else 0)
      | T.EQ -> Const (if n1 = n then 1 else 0)
      | T.AND -> Const (if n1 != 0 && n != 0 then 1 else 0)
      | T.OR -> Const (if n1 != 0 || n != 0 then 1 else 0))
    | Top when op = T.MUL && n = 0 -> Const 0
    | Top -> Top
    | _ -> Bottom)
  | T.ASSIGNU (_, op, y) ->
    let vy = get_val y in
    (match vy with
    | Const n ->
      (match op with
      | T.MINUS -> Const (-n)
      | T.NOT -> Const (if n = 0 then 1 else 0))
    | Top -> Top
    | _ -> Bottom)
  | _ -> Bottom

let join_const_value : const_value -> const_value -> const_value
=fun v1 v2 ->
  match v1, v2 with
  | Bottom, v | v, Bottom -> v
  | Const n1, Const n2 -> if n1 = n2 then Const n1 else Top
  | Top, _ | _, Top -> Top

let join_tables : const_table -> const_table -> const_table
=fun t1 t2 ->
  BatMap.merge (fun _ v1 v2 ->
    match v1, v2 with
    | Some v1, Some v2 -> Some (join_const_value v1 v2)
    | Some v, None | None, Some v -> Some v
    | None, None -> None
  ) t1 t2

let rec run_constant_propagation : program_map -> int -> const_table -> table_map -> table_map
=fun pgm_map current_idx table table_map ->
  if current_idx >= List.length pgm_map then
    table_map
  else
    let current_info = List.find (fun info -> info.idx = current_idx) pgm_map in
    let new_table = 
      match current_info.instr with
      | T.ASSIGNV (x, _, _, _) | T.ASSIGNC (x, _, _, _) | T.ASSIGNU (x, _, _) |
        T.COPY (x, _) | T.COPYC (x, _) | T.LOAD (x, _) | T.READ x ->
        let new_val = eval_const current_info.instr table in
        BatMap.add x new_val table
      | _ -> table
    in
    let process_next table_to_use =
      let prev_table = BatMap.find_opt current_idx table_map in
      let joined_table =
        match prev_table with
        | Some prev -> join_tables table_to_use prev
        | None -> table_to_use
      in
      let new_table_map = BatMap.add current_idx joined_table table_map in
      if prev_table <> None && BatMap.equal (=) (Option.get prev_table) joined_table then
        table_map
      else
        match current_info.instr with
        | T.HALT -> new_table_map
        | T.UJUMP l -> 
          let target_info = List.find (fun info -> info.label = l) pgm_map in
          run_constant_propagation pgm_map target_info.idx joined_table new_table_map
        | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
          let target_info = List.find (fun info -> info.label = l) pgm_map in
          let branch1_map = run_constant_propagation pgm_map target_info.idx joined_table new_table_map in
          run_constant_propagation pgm_map (current_idx + 1) joined_table branch1_map
        | _ -> run_constant_propagation pgm_map (current_idx + 1) joined_table new_table_map
    in
    process_next new_table

let run_reaching_definitions : program_map -> (int, int BatSet.t) BatMap.t =
  fun pgm_map ->
    let n = List.length pgm_map in
    let label_to_pos = List.mapi (fun i info -> (info.label, i)) pgm_map |> List.to_seq |> Hashtbl.of_seq in
    let rda_in = Array.make n BatSet.empty in

    let rec propagate_from idx x def_id visited =
      if idx >= n then ()
      else if Hashtbl.mem visited idx then ()
      else begin
        Hashtbl.add visited idx true;
        let info = List.nth pgm_map idx in
        rda_in.(idx) <- BatSet.add def_id rda_in.(idx);
        if List.mem x info.def then ()
        else begin
          match info.instr with
          | T.HALT -> ()
          | T.UJUMP l ->
            (match Hashtbl.find_opt label_to_pos l with Some next_idx -> propagate_from next_idx x def_id visited | None -> ())
          | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
            let nexts = [
              (match Hashtbl.find_opt label_to_pos l with Some idx -> Some idx | None -> None);
              if idx + 1 < n then Some (idx + 1) else None
            ] |> List.filter_map (fun x -> x) in
            List.iter (fun next_idx -> propagate_from next_idx x def_id visited) nexts
          | _ ->
            if idx + 1 < n then propagate_from (idx + 1) x def_id visited
        end
      end
    in
    List.iteri (fun i info ->
      match info.def, info.def_id with
      | [x], def_id when def_id <> 0 ->
        let visited = Hashtbl.create 16 in
        propagate_from (i + 1) x def_id visited
      | _ -> ()
    ) pgm_map;
    List.mapi (fun i info -> (info.idx, rda_in.(i))) pgm_map
    |> List.to_seq |> BatMap.of_seq

let find_index : ('a -> bool) -> 'a list -> int option
=fun pred lst ->
  let rec aux i = function
    | [] -> None
    | x::xs -> if pred x then Some i else aux (i+1) xs
  in
  aux 0 lst

let run_liveness_analysis : program_map -> liveness_map
=fun pgm_map ->
  let rec fixpoint current_liveness =
    let new_liveness = List.map (fun info ->
      let next_infos = 
        match info.instr with
        | T.HALT -> []
        | T.UJUMP l -> 
          let target_info = List.find (fun i -> i.label = l) pgm_map in
          [target_info]
        | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
          let target_info = List.find (fun i -> i.label = l) pgm_map in
          let next_info = List.find (fun i -> i.idx = info.idx + 1) pgm_map in
          [target_info; next_info]
        | _ -> 
          let next_info = List.find (fun i -> i.idx = info.idx + 1) pgm_map in
          [next_info]
      in
      let out_set = List.fold_left (fun acc next_info -> 
        let next_idx = find_index (fun i -> i.idx = next_info.idx) pgm_map in
        match next_idx with
        | Some idx -> BatSet.union acc (List.nth current_liveness idx).in_set
        | None -> acc
      ) BatSet.empty next_infos in
      let use_set = BatSet.of_list info.use in
      let def_set = BatSet.of_list info.def in
      let in_set = BatSet.union use_set (BatSet.diff out_set def_set) in
      {in_set; out_set}
    ) pgm_map in
    
    if List.for_all2 (fun old new_ -> 
      BatSet.equal old.in_set new_.in_set && BatSet.equal old.out_set new_.out_set
    ) current_liveness new_liveness then
      new_liveness
    else
      fixpoint new_liveness
  in
  let initial_liveness = List.map (fun _ -> {in_set = BatSet.empty; out_set = BatSet.empty}) pgm_map in
  fixpoint initial_liveness

let run_all_analyses : program_map -> analysis_result
=fun pgm_map ->
  let initial_table = BatMap.empty in
  let initial_table_map = BatMap.empty in
  {
    reaching_defs = run_reaching_definitions pgm_map;
    liveness = run_liveness_analysis pgm_map;
    constant_propagation = run_constant_propagation pgm_map 1 initial_table initial_table_map
  }

let string_of_const_value : const_value -> string
=fun v ->
  match v with
  | Bottom -> "⊥"
  | Const n -> string_of_int n
  | Top -> "⊤"

let print_const_table : const_table -> unit
=fun table ->
  BatMap.iter (fun var value ->
    Printf.printf "  %s = %s\n" var (string_of_const_value value)
  ) table

let print_liveness_map : program_map -> liveness_map -> unit
=fun pgm_map liveness_map ->
  List.iter2 (fun info liveness ->
    Printf.printf "Instruction %d: In Set: %s, Out Set: %s\n" 
      info.idx 
      (String.concat ", " (BatSet.to_list liveness.in_set)) 
      (String.concat ", " (BatSet.to_list liveness.out_set))
  ) pgm_map liveness_map

  let print_reaching_defs : (int, int BatSet.t) BatMap.t -> unit
  =fun reaching_defs ->
    BatMap.iter (fun idx defs ->
      Printf.printf "Instruction %d: Defs: %s\n" idx (String.concat ", " (List.map string_of_int (BatSet.to_list defs)))
    ) reaching_defs

let print_analysis_results : program_map -> analysis_result -> unit
=fun pgm_map analyses ->
  print_endline "\n=== Analysis Results ===";
  print_endline "Constant Propagation:";
  BatMap.iter (fun idx table ->
    Printf.printf "\nLabel %d:\n" idx;
    print_const_table table
  ) analyses.constant_propagation;
  print_endline "\nLiveness:";
  print_liveness_map pgm_map analyses.liveness;
  print_endline "\nReaching Definitions:";
  print_reaching_defs analyses.reaching_defs;
  print_endline "\n=====================\n"

let apply_constant_folding : program_map -> analysis_result -> program_map
=fun pgm_map analyses ->
  List.map (fun info ->
    let table = try BatMap.find info.idx analyses.constant_propagation
    with Not_found -> BatMap.empty in
    let new_instr = 
      match info.instr with
      | T.ASSIGNV (x, op, y, z) ->
        (match BatMap.find y table, BatMap.find z table with
        | Const n1, Const n2 ->
          let result = match op with
            | T.ADD -> n1 + n2
            | T.SUB -> n1 - n2
            | T.MUL -> n1 * n2
            | T.DIV -> n1 / n2
            | T.LT -> if n1 < n2 then 1 else 0
            | T.LE -> if n1 <= n2 then 1 else 0
            | T.GT -> if n1 > n2 then 1 else 0
            | T.GE -> if n1 >= n2 then 1 else 0
            | T.EQ -> if n1 = n2 then 1 else 0
            | T.AND -> if n1 != 0 && n2 != 0 then 1 else 0
            | T.OR -> if n1 != 0 || n2 != 0 then 1 else 0
          in
          T.COPYC (x, result)
        | Const n1, _ ->
          (match op with
          | T.ADD -> T.ASSIGNC (x, T.ADD, z, n1)
          | T.MUL -> T.ASSIGNC (x, T.MUL, z, n1)
          | T.LT -> T.ASSIGNC (x, T.GT, z, n1)
          | T.LE -> T.ASSIGNC (x, T.GE, z, n1)
          | T.GT -> T.ASSIGNC (x, T.LT, z, n1)
          | T.GE -> T.ASSIGNC (x, T.LE, z, n1)
          | T.EQ -> T.ASSIGNC (x, T.EQ, z, n1)
          | T.AND -> T.ASSIGNC (x, T.AND, z, n1)
          | T.OR -> T.ASSIGNC (x, T.OR, z, n1)
          | _ -> info.instr)
        | _, Const n2 ->
          (match op with
          | T.ADD -> T.ASSIGNC (x, T.ADD, y, n2)
          | T.SUB -> T.ASSIGNC (x, T.SUB, y, n2)
          | T.MUL -> T.ASSIGNC (x, T.MUL, y, n2)
          | T.DIV -> T.ASSIGNC (x, T.DIV, y, n2)
          | T.LT -> T.ASSIGNC (x, T.LT, y, n2)
          | T.LE -> T.ASSIGNC (x, T.LE, y, n2)
          | T.GT -> T.ASSIGNC (x, T.GT, y, n2)
          | T.GE -> T.ASSIGNC (x, T.GE, y, n2)
          | T.EQ -> T.ASSIGNC (x, T.EQ, y, n2)
          | T.AND -> T.ASSIGNC (x, T.AND, y, n2)
          | T.OR -> T.ASSIGNC (x, T.OR, y, n2))
        | _ -> info.instr)
      | T.ASSIGNC (x, op, y, n) ->
        (match BatMap.find y table with
        | Const n1 ->
          let result = match op with
            | T.ADD -> n1 + n
            | T.SUB -> n1 - n
            | T.MUL -> n1 * n
            | T.DIV -> n1 / n
            | T.LT -> if n1 < n then 1 else 0
            | T.LE -> if n1 <= n then 1 else 0
            | T.GT -> if n1 > n then 1 else 0
            | T.GE -> if n1 >= n then 1 else 0
            | T.EQ -> if n1 = n then 1 else 0
            | T.AND -> if n1 != 0 && n != 0 then 1 else 0
            | T.OR -> if n1 != 0 || n != 0 then 1 else 0
          in
          T.COPYC (x, result)
        | _ -> info.instr)
      | T.ASSIGNU (x, op, y) ->
        (match BatMap.find y table with
        | Const n ->
          let result = match op with
            | T.MINUS -> -n
            | T.NOT -> if n = 0 then 1 else 0
          in
          T.COPYC (x, result)
        | _ -> info.instr)
      | T.COPY (x, y) ->
        (match BatMap.find y table with
        | Const n -> T.COPYC (x, n)
        | _ -> info.instr)
      | _ -> info.instr
    in
    {info with instr = new_instr}
  ) pgm_map

let apply_common_subexpression_elimination : program_map -> analysis_result -> program_map
=fun pgm_map analyses ->
  let n = List.length pgm_map in
  let rda = analyses.reaching_defs in
  let label_to_pos = List.mapi (fun i info -> (info.label, i)) pgm_map |> List.to_seq |> Hashtbl.of_seq in
  let pgm_map_arr = Array.of_list pgm_map in

  let expr_of_instr = function
    | T.ASSIGNV (_, op, y, z) -> Some (`Bop (op, y, z))
    | T.ASSIGNC (_, op, y, n) -> Some (`Bopc (op, y, n))
    | T.ASSIGNU (_, op, y)    -> Some (`Uop (op, y))
    | _ -> None
  in

  let vars_of_expr = function
    | `Bop (_, y, z) -> [y; z]
    | `Bopc (_, y, _) -> [y]
    | `Uop (_, y) -> [y]
  in

  let rec propagate_from idx x expr visited =
    if idx >= n then ()
    else if Hashtbl.mem visited idx then ()
    else begin
      Hashtbl.add visited idx true;
      let info = pgm_map_arr.(idx) in
      let all_vars = x :: vars_of_expr expr in
      if List.exists (fun v -> List.mem v info.def) all_vars then ()
      else begin
        match expr_of_instr info.instr, info.def with
        | Some expr2, [y] when expr2 = expr && x <> y ->
          let rda_in = try BatMap.find info.idx rda with Not_found -> BatSet.empty in
          let safe =
            List.for_all (fun v ->
              let v_def_ids =
                BatSet.filter (fun did ->
                  let def_info = List.find (fun info -> info.def_id = did) pgm_map in
                  List.mem v def_info.def
                ) rda_in
              in
              BatSet.cardinal v_def_ids = 1
            ) all_vars
          in
          if safe then pgm_map_arr.(idx) <- {info with instr = T.COPY (y, x)}
        | _ -> ()
      end;
      match info.instr with
      | T.HALT -> ()
      | T.UJUMP l ->
        (match Hashtbl.find_opt label_to_pos l with Some next_idx -> propagate_from next_idx x expr visited | None -> ())
      | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
        let nexts = [
          (match Hashtbl.find_opt label_to_pos l with Some idx -> Some idx | None -> None);
          if idx + 1 < n then Some (idx + 1) else None
        ] |> List.filter_map (fun x -> x) in
        List.iter (fun next_idx -> propagate_from next_idx x expr visited) nexts
      | _ ->
        if idx + 1 < n then propagate_from (idx + 1) x expr visited
    end
  in

  for i = 0 to n - 1 do
    let info = pgm_map_arr.(i) in
    match expr_of_instr info.instr, info.def with
    | Some expr, [x] ->
      let visited = Hashtbl.create 16 in
      propagate_from (i + 1) x expr visited
    | _ -> ()
  done;
  Array.to_list pgm_map_arr

let apply_copy_propagation : program_map -> analysis_result -> program_map
=fun pgm_map analyses ->
  let n = List.length pgm_map in
  let label_to_pos = List.mapi (fun i info -> (info.label, i)) pgm_map |> List.to_seq |> Hashtbl.of_seq in
  let pgm_map_arr = Array.of_list pgm_map in
  let rda = analyses.reaching_defs in

  let rec propagate_from idx x y def_id visited =
    if idx >= n then ()
    else if Hashtbl.mem visited (idx, x) then ()
    else begin
      Hashtbl.add visited (idx, x) true;
      let info = pgm_map_arr.(idx) in
      if List.mem x info.def then ()
      else begin
        if List.mem x info.use then begin
          let rda_in = try BatMap.find info.idx rda with Not_found -> BatSet.empty in
          let x_def_ids =
            BatSet.filter (fun did ->
              let def_info = List.find (fun info -> info.def_id = did) pgm_map in
              List.mem x def_info.def
            ) rda_in
          in
          if BatSet.cardinal x_def_ids = 1 && BatSet.mem def_id x_def_ids then begin
            let new_instr =
              match info.instr with
              | T.ASSIGNV (z, op, a, b) ->
                let a' = if a = x then y else a in
                let b' = if b = x then y else b in
                T.ASSIGNV (z, op, a', b')
              | T.ASSIGNC (z, op, a, n) ->
                let a' = if a = x then y else a in
                T.ASSIGNC (z, op, a', n)
              | T.ASSIGNU (z, op, a) ->
                let a' = if a = x then y else a in
                T.ASSIGNU (z, op, a')
              | T.COPY (z, a) ->
                let a' = if a = x then y else a in
                T.COPY (z, a')
              | T.LOAD (z, (a, b)) ->
                let a' = if a = x then y else a in
                let b' = if b = x then y else b in
                T.LOAD (z, (a', b'))
              | T.STORE ((a, b), c) ->
                let a' = if a = x then y else a in
                let b' = if b = x then y else b in
                let c' = if c = x then y else c in
                T.STORE ((a', b'), c')
              | T.CJUMP (a, l) ->
                let a' = if a = x then y else a in
                T.CJUMP (a', l)
              | T.CJUMPF (a, l) ->
                let a' = if a = x then y else a in
                T.CJUMPF (a', l)
              | T.WRITE a ->
                let a' = if a = x then y else a in
                T.WRITE a'
              | _ -> info.instr
            in
            pgm_map_arr.(idx) <- {info with instr = new_instr}
          end
        end;
        match info.instr with
        | T.HALT -> ()
        | T.UJUMP l ->
          (match Hashtbl.find_opt label_to_pos l with Some next_idx -> propagate_from next_idx x y def_id visited | None -> ())
        | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
          let nexts = [
            (match Hashtbl.find_opt label_to_pos l with Some idx -> Some idx | None -> None);
            if idx + 1 < n then Some (idx + 1) else None
          ] |> List.filter_map (fun x -> x) in
          List.iter (fun next_idx -> propagate_from next_idx x y def_id visited) nexts
        | _ ->
          if idx + 1 < n then propagate_from (idx + 1) x y def_id visited
      end
    end
  in
  for i = 0 to n - 1 do
    match pgm_map_arr.(i).instr, pgm_map_arr.(i).def with
    | T.COPY (x, y), [x'] when x = x' && pgm_map_arr.(i).def_id <> 0 ->
      let visited = Hashtbl.create 16 in
      propagate_from (i + 1) x y pgm_map_arr.(i).def_id visited
    | _ -> ()
  done;
  Array.to_list pgm_map_arr

let apply_dead_code_elimination : program_map -> analysis_result -> program
=fun pgm_map analyses ->
  let rec convert_to_program acc = function
    | [] -> List.rev acc
    | info::rest ->
      let idx = find_index (fun x -> x.idx = info.idx) pgm_map in
      let liveness = match idx with
        | Some i -> List.nth analyses.liveness i
        | None -> {in_set = BatSet.empty; out_set = BatSet.empty}
      in
      let is_dead = 
        match info.instr with
        | T.ASSIGNV (x, _, _, _) | T.ASSIGNC (x, _, _, _) | T.ASSIGNU (x, _, _) |
          T.COPY (x, _) | T.COPYC (x, _) | T.LOAD (x, _) ->
          not (BatSet.mem x liveness.out_set)
        | _ -> false
      in
      if is_dead then
        convert_to_program acc rest
      else
        convert_to_program ((info.label, info.instr)::acc) rest
  in
  convert_to_program [] pgm_map

let apply_all_optims : program_map -> analysis_result -> program
=fun pgm_map analyses ->
  let opt_pgm_map = apply_constant_folding pgm_map analyses in
  let opt_pgm_map = repair_use_def_chains opt_pgm_map analyses in
  let opt_pgm_map = apply_copy_propagation opt_pgm_map analyses in
  let opt_pgm_map = repair_use_def_chains opt_pgm_map analyses in
  let opt_pgm_map = apply_common_subexpression_elimination opt_pgm_map analyses in
  let opt_pgm = apply_dead_code_elimination opt_pgm_map analyses in
  opt_pgm

let optimize : program -> program
=fun pgm ->
  let rec loop (current_pgm: program) (prev_pgm: program) =
    idx := 0;
    didx := 0;
    let pgm_map = initial_setting current_pgm in
    let analyses = run_all_analyses pgm_map in
    let optimized_pgm = apply_all_optims pgm_map analyses in
    if Hashtbl.hash optimized_pgm = Hashtbl.hash prev_pgm then
      remove_skips optimized_pgm
    else
      loop optimized_pgm current_pgm
  in
  loop pgm []