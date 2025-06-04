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
  use: T.var list;  (* 사용되는 변수들 *)
  def: T.var list;  (* 정의되는 변수들 *)
}

type liveness_info = {
  in_set: T.var BatSet.t;  (* live-in 집합 *)
  out_set: T.var BatSet.t;  (* live-out 집합 *)
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
  (* reaching_defs: unit;  (* 임시로 unit 타입 사용 *) *)
  liveness: liveness_map;  (* liveness analysis 결과 *)
  (* available_expressions: unit;  (* 임시로 unit 타입 사용 *) *)
  constant_propagation: table_map;  (* 각 라벨별 상수 테이블 *)
}

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
  Printf.printf "%d " current_idx;
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
      let new_table_map = BatMap.add current_idx table_to_use table_map in
      match current_info.instr with
      | T.HALT -> new_table_map
      | T.UJUMP l -> 
        let target_info = List.find (fun info -> info.label = l) pgm_map in
        run_constant_propagation pgm_map target_info.idx table_to_use new_table_map
      | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
        let target_info = List.find (fun info -> info.label = l) pgm_map in
        let branch1_map = run_constant_propagation pgm_map target_info.idx table_to_use new_table_map in
        run_constant_propagation pgm_map (current_idx + 1) table_to_use branch1_map
      | _ -> run_constant_propagation pgm_map (current_idx + 1) table_to_use new_table_map
    in
    if current_info.label != 0 then
      try
        let prev_table = BatMap.find current_idx table_map in
        let joined_table = join_tables table prev_table in
        if BatMap.equal (=) prev_table joined_table then
          table_map  (* 고정점에 도달했으므로 현재 맵을 반환 *)
        else
          process_next joined_table
      with Not_found -> 
        process_next new_table
    else
      process_next new_table

(* let run_reaching_definitions : program_map -> unit
=fun _ -> () *)

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

(* let run_available_expressions : program_map -> unit
=fun _ -> () *)

let run_all_analyses : program_map -> analysis_result
=fun pgm_map ->
  let initial_table = BatMap.empty in
  let initial_table_map = BatMap.empty in
  {
    (* reaching_defs = run_reaching_definitions pgm_map; *)
    liveness = run_liveness_analysis pgm_map;
    (* available_expressions = run_available_expressions pgm_map; *)
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
=fun pgm_map _ ->
  pgm_map

let apply_copy_propagation : program_map -> analysis_result -> program_map
=fun pgm_map _ ->
  pgm_map

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
          T.COPY (x, _) | T.COPYC (x, _) | T.LOAD (x, _) | T.READ x ->
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
  print_pgm_map opt_pgm_map;
  let opt_pgm_map = apply_common_subexpression_elimination opt_pgm_map analyses in
  let opt_pgm_map = apply_copy_propagation opt_pgm_map analyses in
  let opt_pgm = apply_dead_code_elimination opt_pgm_map analyses in
  opt_pgm

let optimize : program -> program
=fun pgm ->
  let rec loop (current_pgm: program) (prev_pgm: program) =
    idx := 0;
    didx := 0;
    let pgm_map = initial_setting current_pgm in
    (* print_pgm_map pgm_map; *)
    let analyses = run_all_analyses pgm_map in
    (* print_analysis_results pgm_map analyses;  (* 분석 결과 출력 *) *)
    let optimized_pgm = apply_all_optims pgm_map analyses in

    (* T.pp optimized_pgm; *)

    if Hashtbl.hash optimized_pgm = Hashtbl.hash prev_pgm then
      (T.pp pgm; optimized_pgm)
    else
      loop optimized_pgm current_pgm
  in
  (* let _ = loop pgm [] in
  pgm *)
  loop pgm []