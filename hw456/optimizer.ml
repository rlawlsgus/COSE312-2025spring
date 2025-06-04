open T

let didx = ref 0
let new_def() = didx := !didx + 1; !didx

let idx = ref 0
let new_idx() = idx := !idx + 1; !idx

type var_info = {
  idx: int;
  label: int;
  def_id: int;
  var_name: var;
  instr: T.instr;
}

type program_map = var_info list

type const_value = 
  | Bottom
  | Const of int
  | Top

type const_table = (T.var, const_value) BatMap.t

type analysis_result = {
  reaching_defs: unit;  (* 임시로 unit 타입 사용 *)
  liveness: unit;       (* 임시로 unit 타입 사용 *)
  available_expressions: unit;  (* 임시로 unit 타입 사용 *)
  constant_propagation: const_table list;  (* 각 라벨별 상수 테이블 *)
}

let initial_setting : T.program -> program_map
=fun pgm ->
  List.map (fun (label, instr) ->
    match instr with
    | T.ASSIGNV (x, _, _, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.ASSIGNC (x, _, _, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.ASSIGNU (x, _, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.COPY (x, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.COPYC (x, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.LOAD (x, _) -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | T.READ x -> {idx = new_idx(); label; def_id = new_def(); var_name = x; instr}
    | _ -> {idx = new_idx(); label; def_id = 0; var_name = ""; instr}
  ) pgm

let print_pgm_map : program_map -> unit
=fun pgm_map ->
  let print_var_info info =
    Printf.printf "Idx: %d, Label: %d, Def ID: %d, Var: %s\n" 
      info.idx
      info.label 
      info.def_id 
      info.var_name;
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
    | _ -> Bottom)
  | T.ASSIGNU (_, op, y) ->
    let vy = get_val y in
    (match vy with
    | Const n ->
      (match op with
      | T.MINUS -> Const (-n)
      | T.NOT -> Const (if n = 0 then 1 else 0))
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

let rec run_constant_propagation : program_map -> int -> const_table -> const_table list
=fun pgm_map current_idx table ->
  Printf.printf "%d " current_idx;
  if current_idx >= List.length pgm_map then
    [table]
  else
    let current_info = List.find (fun info -> info.idx = current_idx) pgm_map in
    let new_table = 
      match current_info.var_name with
      | "" -> table
      | x ->
        let new_val = eval_const current_info.instr table in
        BatMap.add x new_val table
    in
    let next_tables = 
      match current_info.instr with
      | T.HALT -> []
      | T.UJUMP l -> 
        let target_info = List.find (fun info -> info.label = l) pgm_map in
        run_constant_propagation pgm_map target_info.idx new_table
      | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
        let target_info = List.find (fun info -> info.label = l) pgm_map in
        let branch1 = run_constant_propagation pgm_map target_info.idx new_table in
        let branch2 = run_constant_propagation pgm_map (current_idx + 1) new_table in
        List.map (join_tables new_table) (branch1 @ branch2)
      | _ -> run_constant_propagation pgm_map (current_idx + 1) new_table
    in
    new_table :: next_tables

let run_reaching_definitions : program_map -> unit
=fun _ -> ()

let run_liveness_analysis : program_map -> unit
=fun pgm_map ->
  let n = List.length pgm_map in
  (* 1. def, use 집합 생성 *)
  let def_use_list = List.map (fun info ->
    let def, use =
      match info.instr with
      | T.ASSIGNV (x, _, y, z) -> [x], [y; z]
      | T.ASSIGNC (x, _, y, _) -> [x], [y]
      | T.ASSIGNU (x, _, y)    -> [x], [y]
      | T.COPY (x, y)          -> [x], [y]
      | T.COPYC (x, _)         -> [x], []
      | T.LOAD (x, (a, i))     -> [x], [a; i]
      | T.STORE ((a, i), x)    -> [], [a; i; x]
      | T.READ x               -> [x], []
      | T.WRITE x              -> [], [x]
      | T.CJUMP (x, _)         -> [], [x]
      | T.CJUMPF (x, _)        -> [], [x]
      | _                      -> [], []
    in
    (def, use)
  ) pgm_map in

  (* 2. in, out 집합 초기화 *)
  let in_sets  = Array.make n BatSet.empty in
  let out_sets = Array.make n BatSet.empty in

  (* 3. succ 계산: 각 인스트럭션의 다음 인덱스(분기 포함) *)
  let succ = Array.init n (fun i ->
    let info = List.nth pgm_map i in
    match info.instr with
    | T.UJUMP l ->
      let l_instr = List.find (fun info' -> info'.label = l) pgm_map in
      let idx_l = l_instr.idx in
      [idx_l]
    | T.CJUMP (_, l) | T.CJUMPF (_, l) ->
      let l_instr = List.find (fun info' -> info'.label = l) pgm_map in
      let idx_l = l_instr.idx in
      if i + 1 < n then [idx_l; i + 1] else [idx_l]
    | T.HALT -> []
    | _ -> if i + 1 < n then [i + 1] else []
  ) in

  (* 4. 고정점 반복 *)
  let changed = ref true in
  while !changed do
    changed := false;
    for i = n - 1 downto 0 do
      let old_in  = in_sets.(i) in
      let old_out = out_sets.(i) in
      let def, use = List.nth def_use_list i in
      let out_i = List.fold_left (fun acc s -> BatSet.union acc in_sets.(s)) BatSet.empty succ.(i) in
      let in_i  = BatSet.union (BatSet.of_list use)
                    (BatSet.diff out_i (BatSet.of_list def)) in
      if not (BatSet.equal in_i old_in) || not (BatSet.equal out_i old_out) then
        changed := true;
      in_sets.(i)  <- in_i;
      out_sets.(i) <- out_i;
    done
  done;

  (* 결과 출력 등은 필요에 따라 추가 *)
  ()

let run_available_expressions : program_map -> unit
=fun _ -> ()

let run_all_analyses : program_map -> analysis_result
=fun pgm_map ->
  let initial_table = BatMap.empty in
  idx := 0;
  {
    reaching_defs = run_reaching_definitions pgm_map;
    liveness = run_liveness_analysis pgm_map;
    available_expressions = run_available_expressions pgm_map;
    constant_propagation = run_constant_propagation pgm_map 1 initial_table;
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

let print_analysis_results : analysis_result -> unit
=fun analyses ->
  print_endline "\n=== Analysis Results ===";
  print_endline "Constant Propagation:";
  List.iteri (fun i table ->
    Printf.printf "\nLabel %d:\n" i;
    print_const_table table
  ) analyses.constant_propagation;
  print_endline "\n=====================\n"

let optimize : program -> program
=fun pgm ->
  let rec loop (current_pgm: program) (prev_pgm: program) =
    let pgm_map = initial_setting current_pgm in
    print_pgm_map pgm_map;
    let analyses = run_all_analyses pgm_map in
    print_analysis_results analyses;  (* 분석 결과 출력 *)
    let optimized_pgm = (* apply_all_optims current_pgm analyses in *) current_pgm in

    T.pp optimized_pgm;

    if Hashtbl.hash optimized_pgm = Hashtbl.hash prev_pgm then
      optimized_pgm
    else
      loop optimized_pgm current_pgm
  in
  loop pgm []